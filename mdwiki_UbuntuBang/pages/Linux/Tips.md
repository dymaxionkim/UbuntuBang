# Tips

## UbuntuBang 설치 자동화 도구
* [Setup_OS](Setup_OS.md) 및 [Setup_Apps](Setup_Apps.md) 문서의 전 과정을 자동화한 도구입니다.
* 이곳( https://github.com/dymaxionkim/UbuntuBang )의 설명대로 따라 하면 됩니다.
<div><img src='https://cloud.githubusercontent.com/assets/12775748/11458100/9000d1e6-96fc-11e5-96a5-0d7cc24c7e91.png' width=800></div>


## 하드디스크 점유율 확인
* 다음 명령을 때려주면 현재 하드디스크의 남은 용량을 확인할 수 있습니다.
```
df -h
```

## 시스템 기본 웹브라우저를 변경하는 명령어
```
sudo update-alternatives --config x-www-browser
```




## 동영상 편집 1

* 가끔 간단한 동영상 편집이 필요할 때가 있는데, 최대한 간단하게 하고 싶을 때는 ffmpeg 유틸리티를 사용하는 것이 좋겠습니다.
* ffmpeg으로 GUI 없이, 터미널에서 직접 명령어를 쳐서 간단히 동영상들을 가지고 편집할 수 있습니다.
* 설치 방법은 다음과 같습니다.  (비공식 저장소를 이용하여 설치)

```
ssudo add-apt-repository ppa:djcj/hybrid
sudo apt-get update
sudo apt-get install ffmpeg
```
또는
```
sudo add-apt-repository ppa:jonathonf/ffmpeg-3
sudo apt-get update
sudo apt-get install ffmpeg libav-tools x264 x265
```

* 사용 방법은 [이곳](http://www.minetech.co.kr/bbs/view.php?id=FREE_BOARD&no=136), [이곳](http://horangi.tistory.com/290), [이곳](http://ffmpeg.org/ffmpeg.html#Video-Options) 등을 참고합니다.

* 몇가지 사용 예제를 봅시다.

```
# 여러개의 동영상을 하나로 합치기 (a.avi + b.avi --> ab.avi)
ffmpeg -i a.avi -i b.avi -vcodec copy -acodec copy -vcodec copy -acodec copy ab.avi -newvideo -newaudio

# 여러개의 동영상을 하나로 합치기 (001.ogv + 002.ogv --> out.ogv)
ffmpeg -i "concat:001.ogv|002.ogv" -c copy out.ogv

# 일정구간 잘라내기 (a.avi에서 600초부터 120초간만 잘라내서 b.avi로 저장)
ffmpeg -i a.avi -ss 600 -t 120 b.avi

# 프레임 레이트 조정 (-an은 오디오 제거 옵션, 1/4은 배속 조절 비율)
ffmpeg -i a.avi -vf "setpts=(1/4)*PTS" -an b.avi

# gif 움짤(?)을 avi 동영상으로 변환
ffmpeg -i a.gif a.avi
```


## 동영상 편집 2

* GUI로 동영상을 편집할 수 있는 소프트웨어는 여러가지가 상당히 많은데, 그중에 윈도우 무비메이커랑 제일 비슷한 느낌으로 사용 가능한 것이 OpenShot 인 것 같습니다. ( http://www.openshotvideo.com/ )

* 설치 방법은 아래와 같습니다. ( http://www.cyberciti.biz/faq/howto-install-openshot-under-debian-ubuntu-linux/ )
```
sudo add-apt-repository ppa:openshot.developers/ppa
sudo apt-get update
sudo apt-get install openshot openshot-doc
```
