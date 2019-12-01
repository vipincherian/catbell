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
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ActnList, ExtCtrls, Buttons, LCLIntf, LCLType,
  observers, clockswidget, settings, optionsform, aboutform;

const
  TICON_RED_INDEX: integer = 1;
  TICON_GREEN_INDEX: integer = 2;

type

  { TMainForm }

  TMainForm = class(TForm)
    aiExport: TAction;
    aiNewAlarm: TAction;
    aiOptions: TAction;
    aiQuit: TAction;
    aiAbout: TAction;
    alMain: TActionList;
    aiNewTimer: TAction;
    ilMain: TImageList;
    ilMainSmall: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    miToolsOptions: TMenuItem;
    miTools: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miAbout: TMenuItem;
    mmMain: TMainMenu;
    miAlarm: TMenuItem;
    miNewTimer: TMenuItem;
    pnlBorder: TPanel;
    pnlClocks: TPanel;
    sdgExport: TSaveDialog;
    sbMoveClockUp: TSpeedButton;
    sbMoveClockDown: TSpeedButton;
    sbDelete: TSpeedButton;
    sbxClocks: TScrollBox;
    tlbMain: TToolBar;
    ToolButton1: TToolButton;
    tiMain: TTrayIcon;
    tbShowTrayAlert: TToolButton;
    tbShowModalAlert: TToolButton;
    tbProgressAuto: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure aiNewAlarmExecute(Sender: TObject);
    procedure aiAboutExecute(Sender: TObject);
    procedure aiNewTimerExecute(Sender: TObject);
    procedure aiOptionsExecute(Sender: TObject);
    procedure aiQuitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbxClocksClick(Sender: TObject);
    procedure tbProgressAutoClick(Sender: TObject);
    procedure tbShowModalAlertClick(Sender: TObject);
    procedure tbShowTrayAlertClick(Sender: TObject);
    procedure tiMainClick(Sender: TObject);

  private
    { private declarations }
    FClockWidget: TClocksWidget;
  public
    { public declarations }
    property Clocks: TClocksWidget read FClockWidget;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin

  //DoubleBuffered:=True;
  FClockWidget := TClocksWidget.Create;
  FClockWidget.ScrollBox := sbxClocks;
  ilMainSmall.GetIcon(TICON_GREEN_INDEX, tiMain.Icon);
  tiMain.Visible := True;

  with GlobalUserConfig do
  begin
    Top := LastPos.Top;
    Left := LastPos.Left;
    Width := LastPos.Width;
    Height := LastPos.Height;

    tbShowModalAlert.Down := ShowModalAlert;
    tbShowTrayAlert.Down := ShowTrayAlert;

    tbProgressAuto.Down:= AutoProgress;
  end;


  //Self.AlphaBlend:=True;
  //Self.AlphaBlendValue:=50;

end;

procedure TMainForm.aiNewTimerExecute(Sender: TObject);
begin

end;

procedure TMainForm.aiOptionsExecute(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TMainForm.aiQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  CurrPos: TRect;
begin
  if GlobalUserConfig.QueryExit then
    CanClose := MessageDlg('Do you really want to close the application?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  //TODO: This should be moved to form close?
  CurrPos.Top := Top;
  CurrPos.Left := Left;
  CurrPos.Right := CurrPos.Left + Width;
  CurrPos.Bottom := CurrPos.Top + Height;
  with GlobalUserConfig do
  begin
    LastPos := CurrPos;
    ShowModalAlert:=tbShowModalAlert.Down;
    ShowTrayAlert:=tbShowTrayAlert.Down;
    AutoProgress:=tbProgressAuto.Down;
  end;


end;

procedure TMainForm.aiAboutExecute(Sender: TObject);
begin
  //ShowMessage(IntToStr(tiMain.Canvas.Width));
  //ShowMessage('Catbell by Vipin Cherian (c) 2017');
  frmAbout.ShowModal;
  //tiMain.
end;

procedure TMainForm.aiNewAlarmExecute(Sender: TObject);
begin

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin

  FClockWidget.Free;
end;

procedure TMainForm.sbxClocksClick(Sender: TObject);
begin

end;

procedure TMainForm.tbProgressAutoClick(Sender: TObject);
begin
  GlobalUserConfig.AutoProgress:=tbProgressAuto.Down;
end;

procedure TMainForm.tbShowModalAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowModalAlert := tbShowModalAlert.Down;
end;

procedure TMainForm.tbShowTrayAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowTrayAlert := tbShowTrayAlert.Down;
end;

procedure TMainForm.tiMainClick(Sender: TObject);
begin

end;


end.
