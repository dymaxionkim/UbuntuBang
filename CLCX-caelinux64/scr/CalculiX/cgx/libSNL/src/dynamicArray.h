// Copyright 2005 Scott A.E. Lanham, Australia.
// --------------------------------------------
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Library General Public License for more details.

// *** Dynamically Allocated Paged Array ***

#ifndef DYNAMICARRAY_H
#define DYNAMICARRAY_H

#define DYNAMICARRAY_DEFAULT_PAGE_SIZE 512

template < class T > class dynamicArray
{
    public:

        virtual ~dynamicArray();

        dynamicArray();
        dynamicArray ( const dynamicArray<T>& arrayToCopy );
        dynamicArray ( int pageSize );

        dynamicArray<T>& operator= ( const dynamicArray<T>& arrayToCopy );

        void copyFrom ( const dynamicArray<T>& arrayToCopy );

        int pageSize();

        T* copyOfElements();

        int sizeAllocated();

        int maxIndex();

        T& operator[] ( int index );

    protected:

        void grow ( int bySize );

    private:

        int     page_size;
        
        T**     array_pages;

        int     num_pages;

        int     array_size;

        int     max_index;  // Maximum index number that has been used.

        T       null_element;
};

template < class T > dynamicArray<T>::~dynamicArray()
{
    if ( array_pages )
    {
        for ( int pageIndex = 0; pageIndex < num_pages; pageIndex ++ )
            delete[] array_pages [ pageIndex ];
    }
}

template < class T > dynamicArray<T>::dynamicArray()
{
    page_size = DYNAMICARRAY_DEFAULT_PAGE_SIZE;
    num_pages = 0;
    array_pages = 0;
    array_size = 0;
    max_index = 0;
}

template < class T > dynamicArray<T>::dynamicArray ( const dynamicArray<T>& arrayToCopy )
{
    // Copy constructor.
    // -----------------

    copyFrom ( arrayToCopy );
}

template < class T > dynamicArray<T>::dynamicArray ( int pageSize )
{
    // Constructor.
    // ------------
    // initialSize:     Initial number of elements in array.
    // pageSize:        Granularity of array size increase. Size is number of elements NOT bytes.

    page_size = pageSize;
    num_pages = 0;
    array_pages = 0;
    array_size = 0;
    max_index = 0;
}

template < class T > dynamicArray<T>& dynamicArray<T>::operator= ( const dynamicArray<T>& arrayToCopy )
{
    // Assignment operator.
    // --------------------

    if ( array_pages )
    {
        for ( int pageIndex = 0; pageIndex < num_pages; pageIndex ++ )
            delete[] array_pages [ pageIndex ];
    }

    copyFrom ( arrayToCopy );
}

template < class T > void dynamicArray<T>::copyFrom ( const dynamicArray<T>& arrayToCopy )
{
    // Copy contents of another dynamic array into this.
    // -------------------------------------------------
    // Notes:   Type <T> must have an assignment operator that doesn't
    //          fuck things up.

    page_size = arrayToCopy.page_size;
    num_pages = arrayToCopy.num_pages;
    array_size = arrayToCopy.array_size;
    max_index = arrayToCopy.max_index;

    if ( ! arrayToCopy.array_pages )
    {
        num_pages = 0;
        array_size = 0;
        max_index = 0;
        array_pages = 0;
    }
    else
    {
        array_pages = new T* [ num_pages ];
    
        for ( int page = 0; page < num_pages; page ++ )
        {
            T* pageToCopy = arrayToCopy.array_pages [ page ];
    
            T* newPage = new T [ page_size ];
    
            array_pages [ page ] = newPage;
            
            for ( int index = 0; index < page_size; index ++ )
                newPage [ index ] = pageToCopy [ index ];
        }
    }
}

template < class T > int dynamicArray<T>::pageSize()
{
    return page_size;
}

template < class T > T* dynamicArray<T>::copyOfElements()
{
    // Return pointer to copy of array elements.
    // -----------------------------------------
    // Notes:       Caller owns returned array.

    if ( ! num_pages ) return 0;

    int arraySize = array_size;

    T* retArray = new T [ arraySize ];

    int retArrayIndex = 0;

    for ( int page = 0; page < num_pages; page ++ )
    {
        T* currentPage = array_pages [ page ];
        
        for ( int index = 0; index < page_size; index ++ )
            retArray [ retArrayIndex ++ ] = currentPage [ index ];
    }

    return retArray;
}

template < class T > int dynamicArray<T>::sizeAllocated()
{
    // Return size of all allocated elements.
    // --------------------------------------

    array_size = num_pages * page_size;

    return array_size;
}

template < class T > int dynamicArray<T>::maxIndex()
{
    // Return maximum index number that has been referenced.
    // -----------------------------------------------------

    return max_index;
}

template < class T > void dynamicArray<T>::grow ( int bySize )
{
    // Grow array.
    // -----------
    // bySize:      Grow by this many elements.
    //
    // Notes:       Will only increase size to page boundaries.

    int growNumPages = bySize / page_size;

    if ( bySize % page_size > 0 )
        growNumPages ++;

    // Grow pages array.

    T** newPageArray = new T* [ num_pages + growNumPages ];

    if ( array_pages )
    {
        for ( int pageIndex = 0; pageIndex < num_pages; pageIndex ++ )
            newPageArray [ pageIndex ] = array_pages [ pageIndex ];

        delete[] array_pages;
    }
    
    // Populate pages array with new pages.

    for ( int pageIndex = 0; pageIndex < growNumPages; pageIndex ++ )
    {
        T* newPage = new T [ page_size ];
        newPageArray [ num_pages + pageIndex ] = newPage;
    }

    array_pages = newPageArray;
    
    num_pages += growNumPages;

    array_size = num_pages * page_size;
}

template < class T > T& dynamicArray<T>::operator[] ( int index )
{
    // Return reference to array element at index.
    // -------------------------------------------
    // index:       Array index.

    if ( index < 0 ) return null_element;  // No negative indices allowed.

    if ( ! array_pages || index >= array_size )
        grow ( index - array_size + 1 );

    if ( index > max_index ) max_index = index;

    int pageIndex = index / page_size;

    int elementIndex = index % page_size;

    return array_pages [ pageIndex ] [ elementIndex ];
}

#endif

