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
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, datetimectrls, main, controller, mainformwidget, clocks,
  observers, sequence, clockswidget, timerframe, settings, optionsform, 
timeralertform, aboutform
  { you can add units after this };

{$R *.res}

var
  AppController: TController;
  FormWidget: TMainFormWidget;
  //DefaultConfig: TDefaultConfig;
begin
  RequireDerivedFormResource:=True;
  Application.Initialize;

  InitSettings;

  FormWidget := TMainFormWidget.Create();
  AppController := TController.Create();
  AppController.ProcessCommandline;

  Application.CreateForm(TMainForm, MainForm);

  //MainFormWidget.FormView.Form := TMainWindow(Application.MainForm);
  FormWidget.Form := TMainForm(Application.MainForm);
  AppController.FormWidget := FormWidget;
  AppController.LoadfromFile;
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmTimerAlert, frmTimerAlert);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.Run;

  AppController.Destroy;
  FormWidget.Destroy;
  CleanupSettings;
end.

