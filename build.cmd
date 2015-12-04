@echo on

set APPVEYOR_CI=1

"packages\FAKE.4.10.3\tools\Fake.exe" Build.fsx