# RedMine

![RedMine](https://raw.githubusercontent.com/docker-library/docs/master/redmine/logo.png)

## 소개
* 레드마인 위키 ::: https://www.redmine.org/projects/redmine/wiki/KoGetting_Started
* 레드마인은 웹서버를 구성해서 제공하는 형태의 유연한 프로젝트 관리 툴입니다.
* 루비온레일즈로 개발되었고, 플랫폼과 데이타베이스를 원하는대로 선택해서 사용 가능합니다.
* 당연히 오픈소스이고 GPL2 입니다.
* 웹서버를 구성해야 하므로 설치방법이 좀 따다로운 것 같은데, Docker로 이미지를 가져와서 그냥 제공한다면 간단하게 셋팅 가능할 것으로 생각됩니다.

## 필요성
* 보통 나이가 좀 있는 중역분들은 기껏해야 MS-Project 정도의 툴만 생각하고, 좀 더 심한 경우에는 엑셀로 간트챠트를 일일이 수동으로 그려서 제공하기를 요구하며, 더 심한 경우에는 파워포인트로 자주 발표/보고하기를 원하는 경우가 많습니다.  사람이 기술을 따라잡지 못하는 현상이죠.
* 또한, 실무자들도 새로운 관리 툴을 사용해야 한다고 강요를 받으면 일거리만 더 늘어나고, 자신의 업무를 마이크로 매니지먼트 당한다고 느끼기 때문에 거부감을 가지고 저항하기도 할 것입니다.
* 아울러, 많은 경우 실제 현업에서는 프로젝트 관리라는게 탁상머리에서 산업공학자들이 생각한 것 처럼 간단하게 흘러가지 않습니다.  경영자들은 자주 프로젝트의 근간을 흔드는 결정을 즉흥적으로 내리는 경우도 많고, 또 기획단계에서 수립한 계획이 무용지물이 되는 경우가 매우 잦습니다.  즉 프로젝트 일정 변경은 매우 빈번하게 발생합니다.
* 소프트웨어 개발 프로젝트가 아니라 나의 경우, 제조업 관련 제품개발 프로젝트에서 이것을 적용해 보면 어떨까 심각하게 고려중입니다.  여러가지 온갖 방법을 이제까지 적용해 봤는데 아직까지 만족스러운 프로젝트 관리도구를 만나지 못한 상태입니다.
* 레드마인은 일단 내가 생각한 요구사항을 모두 빠짐없이 충족하는 것 같습니다.  (다중 프로젝트 관리 가능할 것, 간트챠트 자동으로 그려줄 것, 데이타 저장소와 연계될 것, 웹서비스일 것...)  설치 및 관리의 기술적 어려움을 제외하고는, 사용자 단에서는 그다지 어렵지 않게 느낄 확률이 높다고 생각됩니다.  웹서비스이니까, 웹에서 뭘 하는게 어려워봤자 얼마나 어렵겠습니까.
* 아무튼, 이것의 도입 전제조건은 대충 다음과 같다고 생각됩니다.
  **영감님들의 양보** : 자동으로 생성되는 보고서를 그대로 읽겠다는 양해가 필요합니다.  보고서가 뭐 이렇냐, 좀 이쁘게 표를 이쪽으로 옮기고 제목을 저리로 옮기고 좀 더 글씨를 굵게 해야 되지 않냐, 문구가 이상하다는 등의 형식을 유난히 따지는 영감님들은 그런 습관을 버리셔야 합니다. (라고 실무 팀장이 사장에게 말하는 경우는 발생하지 않겠죠...  하지만 어떻게든 설득 과정은 필요할 듯 합니다.)
  **실무자들의 의지** : 이슈트래킹(일감관리)는 부지런하게 기록해 나가는 습관이 필요합니다.  일일업무보고를 이걸로 대체하던가 하면 어떨까 합니다.
  **프로젝트 매니저의 수준** : 프로젝트 계획의 급격한 변화시 유연하게 대처할 수 있는 노하우가 필요합니다.  이건 꼭 레드마인이 아니더라도 당연히 요구되는 덕목이겠군요.

## 구조설계 분야에 접목할 수 있을까.
* 소스코드와 달리, 기계설계 데이타는 대부분 바이너리 데이타입니다.  이점이 소프트웨어 분야에서 새롭게 등장하는 신기술들을 구조설계 쪽에 접목하는데 걸림돌이 됩니다.  텍스트 문서에 적용하는 버전관리 기법을 그대로 쓸 수 없기 때문이죠.
* 그럼에도 불구하고, 레드마인의 경우에는 단순히 저장소를 링크해 주기만 하면 될 것이므로 많은 기능을 누릴 것을 포기만 한다면 어느정도 접목이 가능하지 않을까 합니다.
* 마일스톤에 해당하는 설계데이타를 날짜별로 묶어서 체계적으로 저장 관리하면 될 테니까요.
* 또다른 걱정거리는, 만일 내가 팀장으로 재직할 때 레드마인으로 구축해서 성공했다 하더라도, 내가 퇴직하고 다음번 후임자가 이 시스템에 적응하지 못할 가능성이 대단히 높다는 점입니다.  지속성이 없을 것이라는 점이죠.  특히 구조설계하는 사람들은 소프트웨어 공학에 굉장히 뒤떨어져 있기 때문에(?) 새로운 소프트웨어를 습득하려는 의욕을 가진 사람을 만나기는 굉장히 힘듭니다.
* 아무튼 아직 좀 더 검토해 볼 필요가 있다고 생각합니다.

## 설치방법
* Native ::: https://opentutorials.org/course/438/2436
* Bitnami RedMine Package ::: https://bitnami.com/stack/redmine
* Docker + RedMine ::: https://translate.google.com/translate?prev=_t&hl=en&ie=UTF-8&u=http://qiita.com/shouta-dev/items/24cfee84ae1d4135ee43&sl=auto&tl=ko
* Docker Image of RedMine ::: https://hub.docker.com/r/sameersbn/redmine/

## 테마 추가 설치
* [flatly_light](https://github.com/Nitrino/flatly_light_redmine), [flat](https://github.com/tsi/redmine-theme-flat), [hardpixel](https://github.com/hardpixel/minelab), [makotokw](https://github.com/makotokw/redmine-theme-gitmike), [PurpleMine2](https://github.com/mrliptontea/PurpleMine2)


## 간트챠트를 그림으로 출력할 때 한글 문제 해결
* Ref ::: http://www.whatwant.com/582
* RedMine 사용중 발견한 문제점으로, Gantt Chart를 PNG 그림파일로 출력할 때 한글이 다 '???' 식으로 깨져서 나옵니다.  폰트 설정 문제이므로 설정을 변경해서 해결할 수 있다고 합니다.
* 다음 설정 파일을 편집기로 열어서 수정하면 됩니다.
```
sudo leafpad /opt/redmine-3.1.2-0/apps/redmine/htdocs/config/configuration.yml
```
요기서 `rmagick_font_path:` 문구를 찾은 다음, 내용을 `rmagick_font_path: /usr/share/fonts/truetype/nanum/NanumGothic.ttf` 으로 바꿔 써 주면 됩니다.
* 그리고 Redmine 매니저를 실행한 후 여기서 아파치 서버를 다시 시작하면 됩니다.
```
sudo /opt/redmine-3.1.2-0/manager-linux-x64.run
```

## RedMine 페이지의 상단 메뉴에서 '도움말' 링크 위치 변경하기
* 디폴트 상태에서는 레드마인 공식 홈페이지의 사용법 주소로 연결됩니다.
* 사내에서 또는 소규모 팀에서 사용할 경우, 별도로 작성한 도움말로 연결해주는 것이 좋을 때가 있을 것 같습니다.
* 이를 변경하려면 설정파일을 다음과 같이 열어서..
```
sudo leafpad /opt/redmine-3.1.2-0/apps/redmine/htdocs/lib/redmine/info.rb
```
* `def help_url; '...'` 항목에 들어있는 주소를 원하는 주소로 변경하면 됩니다.
* 그리고 RedMine 서버 재시작.

## 플러그인 설치 (Monitoring-Controlling 플러그인 설치예)
* Ref. ::: http://egloos.zum.com/keugbang/v/5845069
* Bitnami RedMine 패키지를 설치해서 운용할 경우, 다음의 플러그인 설치 디렉토리로 우선 이동합니다.
```
cd /opt/redmine-3.1.2-0/apps/redmine/htdocs/plugins
```
* 그 다음, 플러그인을 Github에서 곧바로 다운로드 합니다.
```
sudo git clone http://github.com/alexmonteiro/Redmine-Monitoring-Controlling.git redmine_monitoring_controlling
```
* 설치 끝!
* 설치는 끝났으니, 이제 적용하려면 RedMine을 재시작한 다음, 관리 메뉴에서 플러그인이 인식되었는지 확인하면 됩니다.  각 프로젝트의 설정 메뉴의 모듈 탭에서 'Monitoring & Controlling by Project'를 체크해 주면 'Monitoring & Controlling' 메뉴가 생겨납니다.
* 적용 끝!

NOTE: 각종 인터넷 설명문에서 보면, rake라는 명령어를 사용하여 다운로드 받은 플러그인을 인식시켜주도록 설명되어 있는데, 최신버전으로 업데이트 되면서 그 과정이 생략되고 자동으로 인식되는 것으로 생각되네요. Bitnami RedMine에서 rake 같은 명령을 사용하려면, 그냥 일반 터미널 에뮬레이터에서 하면 path가 잡혀있지 않아 해당 명령어를 찾지 못하는데, 그때는 `/opt/redmine-3.1.2-0/use_redmine` 쉘스크립트를 터미널에서 실행시키면 그 안에 내용대로 path 잡아주면서 쉘이 시작됩니다.


## 셋팅 및 데이타 백업하기
* 일감, 문서, 업로드한 파일들의 백업을 위해서는 다음 경로를 통째로 복사해서 백업합니다.
```
/opt/redmine-3.1.2-0/apps/redmine/htdocs/files
```
* 데이타베이스 백업을 위해서는 몇가지 절차가 필요합니다.
* 우선 다음 파일을 복사해서 백업하고, 편집기로 열어서 database, username, password 정보를 파악해 둡니다.
```
/opt/redmine-3.1.2-0/apps/redmine/htdocs/config/database.yml
```
* 이제 다음 경로로 이동해서 데이타베이스 셋팅 명령어를 입력합니다. (이때 username, database 부분은 위에서 파악한 내용으로 대체해서 적어줍니다.  아울러, 이때 RedMine의 데이타베이스는 실행중이어야 접속해서 백업이 이루어지게 되므로 유의합니다.)
```
cd /opt/redmine-3.1.2-0/mysql/bin
sudo ./mysqldump -u username -p database > /home/backup.sql
```
* 그러면 `backup.sql`으로 데이타베이스 셋팅 백업파일이 생깁니다.


## 셋팅 및 데이타 복원하기
* 새로 설치된 RedMine 경로를 확인해 둡니다.  (여기서는 `/opt/redmine-3.1.2-0`으로 설치했다고 합시다.)  여기에 앞서 백업한 셋팅 및 데이타를 심어넣고자 합니다.
* 새로 설치된 RedMine의 데이타베이스 셋팅을 파악합니다.  절차는 위와 같습니다.  즉, 다음 파일을 편집기로 열어서 database, username, password 정보를 파악해 둡니다.
```
/opt/redmine-3.1.2-0/apps/redmine/htdocs/config/database.yml
```
* 그리고 앞서 생성해서 백업해 뒀던 `backup.sql` 데이타베이스 셋팅 백업파일을 `/home` 경로로 복사해 넣습니다. (다른 장소라도 상관없음)
* 이제 다음 경로로 이동해서 데이타베이스 셋팅 명령어를 입력합니다. (이때 username, database 부분은 직전에 파악한 내용으로 대체해서 적어줍니다.  아울러, 이때 RedMine의 데이타베이스는 실행중이어야 접속이 이루어지게 되므로 유의합니다.)
```
cd /opt/redmine-3.1.2-0/mysql/bin
sudo ./mysql -u username -p database < /home/backup.sql
```
* 그러면 데이타베이스 셋팅이 먹어들어가게 됩니다.
* 이제 앞서 백업해 뒀던 일감, 문서, 업로드한 파일들의 백업을 위해서는 다음 경로를 통째로 복사해서 다음 경로에 풀어넣습니다.
```
/opt/redmine-3.1.2-0/apps/redmine/htdocs/files
```
* 기존에 사용하던 각종 플러그인 같은 것들도 알아서 다시 설치해 주면 됩니다.



## Ref.
* http://www.damduck01.com/category/%EB%A0%88%EB%93%9C%EB%A7%88%EC%9D%B8
* http://www.redmine.or.kr
* 레드마인 커뮤니티 ::: http://www.jenkins.or.kr/projects/community
* 레드마인 플러그인 리스트 ::: http://www.redmine.org/projects/redmine/wiki/Plugin_list
* 데이타 백업 ::: http://real-1.tistory.com/entry/redmine-data-%EB%B0%B1%EC%97%85-%EB%B0%8F-%EB%B3%B5%EC%9B%90
