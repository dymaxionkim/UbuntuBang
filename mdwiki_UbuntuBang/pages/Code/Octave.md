
## GNU Octave

* 여러가지 최신 좋은 도구들이 많이 나왔지만, 오래되고 가장 편한 것은 아무래도 Octave 같습니다.
* Octave 버전 3.8부터 자체적인 GUI가 내장되었다고 합니다.
* 현재는 4.0.1까지 버전이 올라갔는데, 이렇게 최신버전을 받기 위해 ppa를 추가합니다.
* 그냥 버전 3.8을 사용하려면 ppa를 추가하지 않고 우분투 저장소를 이용하면 됩니다.


```
sudo add-apt-repository ppa:octave/stable
sudo apt-get update
sudo apt-get install octave
```

* [런치패드](https://launchpad.net/~octave/+archive/ubuntu/stable) 에 가 보면 포함되는 패키지들이 각각 어떤 것들이 있는지 쉽게 확인 할 수 있습니다.
* 아울러 참고로, 공식 윈도우용 [배포판](ftp://ftp.gnu.org/gnu/octave/windows/octave-4.0.1-installer.exe)도 받을 수 있습니다.
* GUI로 실행시키려면 다음 명령어를 치면 됩니다.

```
octave --force-gui
```

