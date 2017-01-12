# FGPG

## Fine Gear Profile Generator
* FGPG는 인볼류트 기어 형상을 엄밀하게 작도하고, 유한요소해석 및 CAD 모델링으로 연계될 수 있도록 데이타를 만들어주는 Julia 스크립트입니다.
* 작동을 위해서는 Julia 및 가급적 IJulia가 필요합니다.
* 유한요소해석을 위해서는 Elmer 및 GMSH가 필요합니다.

## 설치 방법
```
git clone https://github.com/dymaxionkim/FGPG.git
```

## README.md
_Fine Involute Gear Profile Generator ... in Julia Language_

![case01.svg](http://1.bp.blogspot.com/-F_kEDwR4niE/VW8n2-qTsiI/AAAAAAAAcJA/iV22T-clb6U/s640/20150603_006.png)

![case02.svg](http://1.bp.blogspot.com/-r8eonu7ySqY/VW8n2peQToI/AAAAAAAAcI8/wmAs91TuN6g/s640/20150603_005.png)


* Source Code : [FGPG_V11.ipynb](http://nbviewer.ipython.org/gist/dymaxionkim/fe9015463cd41cfb3f40)

* Input Data File : [input4fgpg.csv](https://github.com/dymaxionkim/FGPG/blob/master/input4fgpg.csv)

## How to use
1. Just go to http://juliabox.org
2. Just Log in and Make new Working Directory.
3. Upload FGPG_V11.ipynb & input4fgpg.csv file into the directory.
4. Open FGPG_V11.ipynb.
5. Ctrl+Enter in the Source Code's Cell.
6. Wait.
7. Check the directory.
8. Download Output files you need.
9. Use them for AutoCAD/DraftSight, FreeCAD/CREO/CATIA.., ElmerFEM, GMSH ...

## 사용방법 (How to use in Korean)
* <http://dymaxionkim.blogspot.kr/search/label/GPG>

#### License : GPL3
