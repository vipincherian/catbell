{

Copyright (C) 2017 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}

program catbell;

{$mode objfpc}{$H+}
//{$INTERFACES CORBA}
uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  Dialogs,
  datetimectrls,
  main,
  sequence,
  timerframe,
  settings,
  optionsform,
  timeralertform,
  aboutform,
  editform,
  adjustform,
  audio,
  eventlog,
  metronome
  { you can add units after this }
  {$IF defined(windows) }  ,
  Windows
  {$ENDIF}
  { There is a semicolon here };

{$R *.res}
const
  APP_UUID = '8e746524-2d28-11ea-978f-2e728ce88125';

var
  //AppController: TController;
  //FormWidget: TMainForm;
  //DefaultConfig: TDefaultConfig;
  MutexHandle: THandle;
begin
  //RequireDerivedFormResource := True;

  { Checks to ensure that only one instance of the application runs at a time }
  {$IF defined(windows) }
  MutexHandle := CreateMutex(nil, True, APP_UUID);
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    ShowMessage('Another instance of the application is already running. ' +
      'Cannot run multiple instances.');
    Exit;
  end;
  {$ENDIF}

  {$if declared(useHeapTrace)}
  {$IFOPT D+}
  setHeapTraceOutput('catbell_trace.log');
  {$ENDIF}
  {$endIf}

  Logger := TEventLog.Create(nil);
  Logger.LogType := ltFile;
  Logger.FileName := 'catbell.log';
  //Logger.Set
  Logger.Info('************************');
  Logger.Info('Application run starting');
  Logger.Info('************************');
  Logger.Info('  Build on -       ' + {$INCLUDE %DATE%});
  Logger.Info('  Target CPU -     ' + {$INCLUDE %FPCTARGETCPU%});
  Logger.Info('  Target OS -      ' + {$INCLUDE %FPCTARGETOS%});
  Logger.Info('  FPC version -    ' + {$INCLUDE %FPCVERSION%});

  Application.Initialize;

  InitSettings;

  //FormWidget := TMainFormWidget.Create();
  //AppController := TController.Create();

  Application.CreateForm(TfrmMain, frmMain);
  frmMain.ProcessCommandline;

  //MainFormWidget.FormView.Form := TMainWindow(Application.MainForm);
  //FormWidget.Form := TMainForm(Application.MainForm);
  //AppController.FormWidget := MainForm;

  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmAlert, frmAlert);
  Application.CreateForm(TfrmAbout, frmAbout);
  frmMain.LoadfromFile;
  Application.CreateForm(TfrmEdit, frmEdit);
  Application.CreateForm(TfrmAdjust, frmAdjust);
  Application.Run;

  //  AppController.Destroy;
  //FormWidget.Destroy;
  CleanupSettings;
  {$IF defined(windows) }
  if MutexHandle <> 0 then
    CloseHandle(MutexHandle);
  {$ENDIF}
  Logger.Free;
end.
