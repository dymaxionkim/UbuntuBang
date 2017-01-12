

# Synthesis for Recoil Springs

## Hypothesis
* 압축스프링만 사용 (인장스프링 역할은 없음)
* 질량체의 전후에 모두 스프링 적용 (충격 후 복귀시에도 완충 되도록)

## Synthesis
* 배치 순서 : k1 - m - k2
* 변위 : 좌에서 우측으로 +x 방향

![](http://www.ux1.eiu.edu/~cfadd/1150-05/Hmwk/Ch13/Images/D13.2b.gif)

## Analysis
* 플러스 방향으로 \\(x_1\\) 만큼 변위 발생할 때의 합력을 구해보면

$$
f_1 = k_1 ( x_0 - x_1 )
$$

$$
f_1 = k_2 ( x_0 + x_1 )
$$

$$
f_{total} = f_1 + f_2 = ( k_1 + k_2 ) x_0 + ( k_2 - k_1 ) x_1
$$

## Conditions

* Conditions 1 : \\( k_1 = k_2 \\)

$$( k_2 - k_1 ) x_1 = 0$$

$$f_{total} = ( k_1 + k_2 ) x_0 = Constant$$

* Conditions 2 : \\( k_1 > k_2 \\)

  + \\(f_{total}\\)이 줄어들게 됨

* Condition 3 : \\( k_1 < k_2 \\)

  + \\(f_{total}\\)이 커지게 됨


## Conclusion

* 더 강한 Recoil Force를 얻어내기 위해서는...
  1. 뒤에서 받쳐주는 스프링이 앞쪽보다 더 강하거나 같아야 한다. : \\(k_1 =< k_2\\)
  2. 앞뒤 스프링 사이의 합력 균형점을 찾기 쉽도록 하려면 스프링을 동일하게 가는 것도 좋다.
  2. 스프링 설치 변위를 가급적 최대한 키운다. : \\(x_0\\)
  3. 각각의 스프링 상수는 총 스프링 개수만큼 나누어 분배 가능하다.
  
  



[gimmick:gist](adf22c7c98696ffca4be)

dds

