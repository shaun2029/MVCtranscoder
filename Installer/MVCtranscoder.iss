; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "MVCtranscoder"
#define MyAppVersion "1.0"
#define MyAppPublisher "Immutablefix"
#define MyAppURL "https://github.com/shaun2029/MVCtranscoder"
#define MyAppExeName "MVCTranscoder.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{05AD8D66-8E95-4B44-A33A-773645501489}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf64}\{#MyAppName}
DisableProgramGroupPage=yes
LicenseFile=..\..\\MVCtranscoder\LICENSE
InfoBeforeFile=..\..\\MVCtranscoder\README.md
OutputDir=..\..\\MVCtranscoder\Installer\output
OutputBaseFilename=MVCtranscoder 1.0 Setup
SetupIconFile=..\..\\MVCtranscoder\artwork\Icon_128.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\\MVCtranscoder\MVCTranscoder.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\\MVCtranscode\x64\Release\libmfxsw64.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\\MVCtranscode\x64\Release\MVCtranscode.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\\MVCtranscoder\README.md"; DestDir: "{app}"; DestName: "README"; Flags: ignoreversion
Source: "..\..\\MVCtranscoder\LICENSE"; DestDir: "{app}"; DestName: "LICENCE"; Flags: ignoreversion
Source: "..\..\\MVCtranscode\LICENSE"; DestDir: "{app}"; DestName: "MVCtranscode_LICENCE"; Flags: ignoreversion
Source: "..\..\\MVCtranscode\README.md"; DestDir: "{app}"; DestName: "MVCtranscode_README"; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

