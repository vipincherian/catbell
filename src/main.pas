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
  observers, clockswidget, settings, optionsform, aboutform, BGRABitmap,
  BGRABitmapTypes, FPimage, timeralertform, dateutils, clocks, jsonConf;

const
  TICON_RED_INDEX: integer = 1;
  TICON_GREEN_INDEX: integer = 2;
  TRAY_PROGRESS_ICON_COUNT = 24;

  TRAY_OUTLINE_COLOUR = $3333f2;//$5A9E60;
  TRAY_BG_COLOUR = $c6c6f5;//$DAEADB;
  TRAY_CENTRE_COLOUR = TRAY_BG_COLOUR;
  PROGRESS_COLOUR = $7a2a41;//1A1AB0;

  PROGRESS_COMPLETED = 2.0;

  TRAY_OUTLINE_INSET = 2;
  TRAY_CENTRE_INNER_RADIUS = 2;
  TRAY_CENTRE_OUTER_RADIUS = 4;
  TRAY_BASE_WIDTH = 16;

  RAD_MULTIPLIER = 16;

  APP_ICON_SIZE = 256;

  LAST_TRAY_ICON_DEFAULT = -1;

type

  { TMainForm }

  TMainForm = class(TForm, ITimerObserver)
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

  private
    { private declarations }
    FTrayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FAppProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FTrayStoppedBitmap, FAppStoppedBitmap: TIcon;
    FLastTrayIconIndex: integer;
    FClockWidget: TClocksWidget;
    FClocks: TClocks;
    FDbFileName: string;
    FDbDefault: boolean;
    function GetClocks: TClocksWidget;
    procedure DrawBaseIconBackground(Bmp: TBGRABitmap);
    procedure DrawBaseIconForeground(Bmp: TBGRABitmap);
    //procedure SetDeleteEnabled(AValue: boolean);
    //procedure SetForm(AValue: TMainForm);
    //procedure SetMoveDownEnabled(AValue: boolean);
    //procedure SetMoveUpEnabled(AValue: boolean);
    procedure FormShow(Sender: TObject);
    procedure UpdateAlertFormSettings;
    procedure ExportToFile(Sender: TObject);
    procedure PostTimerCreation(AValue: TTimerClock);
    procedure SetListButtonsStatus;
  public
    { public declarations }
    OnNewTimer: TNotifyEvent;
    OnNewAlarm: TNotifyEvent;
    OnClockDelete: TNotifyEvent;
    OnClockMoveUp: TNotifyEvent;
    OnClockMoveDown: TNotifyEvent;
    OnEXport: TNotifyEvent;
    //property Clocks: TClocksWidget read FClockWidget;
    procedure NewTimerAdded(Sender: TObject);
    procedure NewAlarmAdded(Sender: TObject);
    procedure ClockDeleted(Sender: TObject);
    procedure ClockMovedUp(Sender: TObject);
    procedure ClockMovedDown(Sender: TObject);
     procedure ClockSelected(Sender: TObject);
    procedure TimerFinished(Id: integer);
    procedure ProgressUpdate(Progress: single);
    //procedure OptionsEdit(Sender: TObject);
    procedure ExportClicked(Sender: TObject);
    procedure OptionsFormClosed();
    function GetExportFileName: string;
    //property Form: TMainForm read FForm write SetForm;
    //property Clocks: TClocksWidget read GetClocks;
    //property DeleteEnabled: boolean write SetDeleteEnabled;
    //property MoveUpEnabled: boolean write SetMoveUpEnabled;
    //property MoveDownEnabled: boolean write SetMoveDownEnabled;
    procedure ProcessCommandline;
    procedure SavetoFile;
    procedure LoadfromFile;
    //property Clocks: TClocks read FClocks;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  Count: integer;
  bmp: TBGRABitmap;
  TrayIconSize: integer;
  InSet: integer;
begin

  OnNewTimer := nil;
  OnEXport := nil;
  FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;

  FClocks := TClocks.Create;
  FDbDefault := True;

  //DoubleBuffered:=True;
  FClockWidget := TClocksWidget.Create;
  FClockWidget.ScrollBox := sbxClocks;

  FClocks.Widget := FClockWidget;
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

    tbProgressAuto.Down := AutoProgress;
  end;


  //Self.AlphaBlend:=True;
  //Self.AlphaBlendValue:=50;
  {TODO: Add these to the form}
  aiNewTimer.OnExecute := @NewTimerAdded;
  aiNewAlarm.OnExecute := @NewAlarmAdded;
  //aiOptions.OnExecute := @OptionsEdit;
  sbDelete.OnClick := @ClockDeleted;
  sbMoveClockUp.OnClick := @ClockMovedUp;
  sbMoveClockDown.OnClick := @ClockMovedDown;
  aiExport.OnExecute := @ExportClicked;
  OnShow := @FormShow;


  TrayIconSize := TRAY_BASE_WIDTH;
  TrayIconSize := GetSystemMetrics(SM_CXSMICON);

  bmp := TBGRABitmap.Create(TrayIconSize, TrayIconSize, BGRAPixelTransparent);

  DrawBaseIconBackground(bmp);
  DrawBaseIconForeground(bmp);

  FTrayStoppedBitmap := TIcon.Create;
  FTrayStoppedBitmap.Assign(bmp.Bitmap);

  bmp.Free;


  bmp := TBGRABitmap.Create(APP_ICON_SIZE, APP_ICON_SIZE, BGRAPixelTransparent);
  DrawBaseIconBackground(bmp);
  DrawBaseIconForeground(bmp);

  FAppStoppedBitmap := TIcon.Create;
  FAppStoppedBitmap.Assign(bmp.Bitmap);

  bmp.Free;

  ProgressUpdate(PROGRESS_COMPLETED);

  InSet := 2;

  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin

    bmp :=
      TBGRABitmap.Create(TrayIconSize, TrayIconSize, BGRAPixelTransparent);

    DrawBaseIconBackground(bmp);
    with bmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie(InSet, InSet, (TRAY_BASE_WIDTH - InSet),
        (TRAY_BASE_WIDTH - InSet), 90 * RAD_MULTIPLIER,
        -(15 * RAD_MULTIPLIER * (Count - 1)));
    end;
    DrawBaseIconForeground(bmp);
    FTrayProgressIcons[Count] := TIcon.Create;

    FTrayProgressIcons[Count].Assign(bmp.Bitmap);

    bmp.Free;

    bmp :=
      TBGRABitmap.Create(APP_ICON_SIZE, APP_ICON_SIZE, BGRAPixelTransparent);
    DrawBaseIconBackground(bmp);
    with bmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH,
        (Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH,
        (APP_ICON_SIZE - ((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH)),
        (APP_ICON_SIZE - ((Inset * APP_ICON_SIZE) div TRAY_BASE_WIDTH)),
        90 * RAD_MULTIPLIER, -(15 * RAD_MULTIPLIER * (Count - 1)));
    end;
    DrawBaseIconForeground(bmp);
    FAppProgressIcons[Count] := TIcon.Create;
    FAppProgressIcons[Count].Assign(bmp.Bitmap);

    bmp.Free;
  end;


end;

procedure TMainForm.aiNewTimerExecute(Sender: TObject);
begin

end;

procedure TMainForm.aiOptionsExecute(Sender: TObject);
begin
  //ShowMessage('Hehe');
  frmOptions.ShowModal;
  OptionsFormClosed;
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
    ShowModalAlert := tbShowModalAlert.Down;
    ShowTrayAlert := tbShowTrayAlert.Down;
    AutoProgress := tbProgressAuto.Down;
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
var
  Count: integer;
begin
  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin
    FTrayProgressIcons[Count].Free;
    FAppProgressIcons[Count].Free;
  end;
  FTrayStoppedBitmap.Free;
  FAppStoppedBitmap.Free;

  //inherited Destroy;
  SavetoFile;
  FClocks.Destroy;
  FClockWidget.Free;
  end;

procedure TMainForm.sbxClocksClick(Sender: TObject);
begin

end;

procedure TMainForm.tbProgressAutoClick(Sender: TObject);
begin
  GlobalUserConfig.AutoProgress := tbProgressAuto.Down;
end;

procedure TMainForm.tbShowModalAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowModalAlert := tbShowModalAlert.Down;
end;

procedure TMainForm.tbShowTrayAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowTrayAlert := tbShowTrayAlert.Down;
end;

function TMainForm.GetClocks: TClocksWidget;
begin
  Result := FClockWidget;
end;

procedure TMainForm.DrawBaseIconBackground(Bmp: TBGRABitmap);
var
  TrayInset{, InnerRadius, OuterRadius}: integer;
  Size: integer;
begin
  Size := Bmp.Width;
  TrayInset := (TRAY_OUTLINE_INSET * Size) div TRAY_BASE_WIDTH;
  //InnerRadius := (TRAY_CENTRE_INNER_RADIUS * Size) div TRAY_BASE_WIDTH;
  //InnerRadius := (TRAY_CENTRE_Inner_RADIUS * Size) div TRAY_BASE_WIDTH;
  //Bmp := TBGRABitmap.Create(Size, Size, BGRAPixelTransparent);
  //Bmp.SetSize(Size, Size);
  with Bmp.CanvasBGRA do
  begin
    Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse(0, 0, Size, Size);
    Brush.Color := TRAY_BG_COLOUR;
    Pen.Color := TRAY_BG_COLOUR;
    Ellipse(TrayInset, TrayInset, Size - TrayInset, Size - TrayInset);
    {Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius);
    Brush.Color := TRAY_BG_COLOUR;
    Pen.Color := TRAY_BG_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius); }
  end;
end;

procedure TMainForm.DrawBaseIconForeground(Bmp: TBGRABitmap);
var
  InnerRadius, OuterRadius: integer;
  Size: integer;
begin
  Size := Bmp.Width;
  InnerRadius := (TRAY_CENTRE_INNER_RADIUS * Size) div TRAY_BASE_WIDTH;
  OuterRadius := (TRAY_CENTRE_OUTER_RADIUS * Size) div TRAY_BASE_WIDTH;
  with Bmp.CanvasBGRA do
  begin
    Brush.Color := TRAY_OUTLINE_COLOUR;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Ellipse((Size div 2) - OuterRadius, (Size div 2) - OuterRadius,
      (Size div 2) + OuterRadius, (Size div 2) + OuterRadius);
    Brush.Color := TRAY_CENTRE_COLOUR;
    Pen.Color := TRAY_CENTRE_COLOUR;
    Ellipse((Size div 2) - InnerRadius, (Size div 2) - InnerRadius,
      (Size div 2) + InnerRadius, (Size div 2) + InnerRadius);

    Brush.Color := clWhite;
    Brush.Opacity := 80;
    Pen.Color := clWhite;
    Pen.Opacity := 80;
    Pie(0, 0, Bmp.Width, Bmp.Width, 90 * RAD_MULTIPLIER, -(180 * RAD_MULTIPLIER));

    Brush.Color := TRAY_OUTLINE_COLOUR;
    Brush.Opacity := 50;
    Pen.Color := TRAY_OUTLINE_COLOUR;
    Pen.Opacity := 50;
    Pie(0, 0, Bmp.Width, Bmp.Width, -(60 * RAD_MULTIPLIER), (120 * RAD_MULTIPLIER));
  end;
end;

{*
procedure TMainForm.SetDeleteEnabled(AValue: boolean);
begin
  sbDelete.Enabled := AValue;
end;

procedure TMainForm.SetMoveDownEnabled(AValue: boolean);
begin
  sbMoveClockDown.Enabled := AValue;
end;

procedure TMainForm.SetMoveUpEnabled(AValue: boolean);
begin
  sbMoveClockUp.Enabled := Avalue;
end;*}

procedure TMainForm.FormShow(Sender: TObject);
begin
  //frmOptions.OnClose := @OptionsFormClosed;
  UpdateAlertFormSettings;
end;

procedure TMainForm.UpdateAlertFormSettings;
begin
  with GlobalUserConfig do
  begin
    frmTimerAlert.Color := ModalBackgroundColour;
    frmTimerAlert.stxMessage.Font.Color := ModalCaptionColour;
    frmTimerAlert.stxAdditional.Font.Color := ModalSubtextColour;
  end;
end;

procedure TMainForm.ExportToFile(Sender: TObject);
var
  FileName: string;
  Conf: TJSONConfig;
begin
  SavetoFile;
  FileName := '';
  FileName := GetExportFileName;
  if FileName <> '' then
  begin
    Conf := TJSONConfig.Create(nil);
    FClocks.SaveClocks(Conf);
    Conf.Filename := FileName;
    Conf.Free;
  end;
end;

procedure TMainForm.PostTimerCreation(AValue: TTimerClock);
begin
  AValue.AddSubscription(Self);
  AValue.Widget.OnSelect := @ClockSelected;
end;

procedure TMainForm.SetListButtonsStatus;
begin
  sbDelete.Enabled := FClocks.AnySelected;
  sbMoveClockDown.Enabled:=FClocks.Widget.CanSelectedMovDown;
  sbMoveClockUp.Enabled:= FClocks.Widget.CanSelectedMoveUp;
end;

procedure TMainForm.NewTimerAdded(Sender: TObject);
var
  Added: TTimerClock;
begin
  {*if OnNewTimer <> nil then
    OnNewTimer(Sender);*}
  Added := FClocks.AddTimer;
  PostTimerCreation(Added);
end;

{TODO: This method NewAlarmAdded is not needed}
procedure TMainForm.NewAlarmAdded(Sender: TObject);
begin
  if OnNewAlarm <> nil then
    OnNewAlarm(Sender)
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnNewAlarm was found to be Nil');
end;

procedure TMainForm.ClockDeleted(Sender: TObject);
begin
  //ShowMessage('Haha');
  {*if OnClockDelete <> nil then
  begin
    OnClockDelete(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockDelete was found to be Nil');*}
  SetListButtonsStatus;
end;

procedure TMainForm.ClockMovedUp(Sender: TObject);
begin
  {*if OnClockMoveUp <> nil then
  begin
    OnClockMoveUp(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockMoveUp was found to be Nil');*}
  FClocks.Widget.MoveSelectedClocksUp;
  SetListButtonsStatus;
end;

procedure TMainForm.ClockMovedDown(Sender: TObject);
begin
  {*if OnClockMoveDown <> nil then
  begin
    OnClockMoveDown(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnClockMoveDown was found to be Nil');*}
  FClocks.Widget.MoveSelectedClocksDown;
  SetListButtonsStatus;
end;

procedure TMainForm.ClockSelected(Sender: TObject);
begin
  SetListButtonsStatus;
end;

procedure TMainForm.TimerFinished(Id: integer);
var
  Hours: word;
  Minutes: word;
  Seconds: word;
  Widget: TTimerClockWidget;
  Duration: TDateTime;
begin
  //DecodeTime(Duration, Hours, Minutes, Seconds, Millis);

  Widget := FClockWidget.GetClock(Id);
  Duration := Widget.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  if GlobalUserConfig.ShowTrayAlert then
  begin
    tiMain.BalloonHint :=
      Widget.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    tiMain.ShowBalloonHint;
  end;

  if GlobalUserConfig.ShowModalAlert then
  begin
    frmTimerAlert.stxAdditional.Caption :=
      Widget.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    if not frmTimerAlert.Showing then
      frmTimerAlert.ShowModal;
  end;

end;

procedure TMainForm.ProgressUpdate(Progress: single);
var
  //Bmp: TBitmap;
  Index: integer;
begin
  if (Progress > 1.99) and (Progress < 2.01) then
  begin
    tiMain.Icon.Assign(FTrayStoppedBitmap);
    Icon.Assign(FAppStoppedBitmap);
    Application.Icon.Assign(FAppStoppedBitmap);
    FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;

  end
  else
  begin
    // TODO: Which is correct? Round ro Trunc?
    Index := Trunc(Progress * 24.0);
    if Index = 24 then
      Index := 23;
    Assert((Index >= 0) and (Index < TRAY_PROGRESS_ICON_COUNT));
    if FLastTrayIconIndex <> Index then
    begin
      tiMain.Icon.Assign(FTrayProgressIcons[Index + 1]);
      //FForm.tiMain.Icon.Handle:=FTrayProgressIcons[Index + 1].Handle;
      Icon.Assign(FAppProgressIcons[Index + 1]);
      //FForm.Icon.Handle := FTrayProgressIcons[Index + 1].Handle;
      Application.Icon.Assign(FAppProgressIcons[Index + 1]);
      FLastTrayIconIndex := Index;
    end;
  end;

end;

{8procedure TMainForm.OptionsEdit(Sender: TObject);
begin
  frmOptions.ShowModal;
end;*}

procedure TMainForm.ExportClicked(Sender: TObject);
begin
  if OnExport <> nil then
  begin
    OnExport(Self);
  end
  else
    ShowMessage('Unexpected error at ' +
{$I %FILE%}
      +' ' +
{$I %LINE%}
      +': OnExport was found to be Nil');
end;

procedure TMainForm.OptionsFormClosed();//Sender: TObject; var Action: TCloseAction);
begin

  {Once the options form is closed, tool buttons have to be set according
  to the changed options}
  with GlobalUserConfig do
  begin
    if tbShowModalAlert.Down <> ShowModalAlert then
    begin
      tbShowModalAlert.Down := ShowModalAlert;
      tbShowModalAlertClick(Self);
    end;

    if tbShowTrayAlert.Down <> ShowTrayAlert then
    begin
      tbShowTrayAlert.Down := ShowTrayAlert;
      tbShowTrayAlertClick(Self);
    end;

    if tbProgressAuto.Down <> AutoProgress then
    begin
      tbProgressAuto.Down := AutoProgress;
      tbProgressAutoClick(Self);
    end;
  end;

  UpdateAlertFormSettings;
end;

function TMainForm.GetExportFileName: string;
begin
  if sdgExport.Execute then
    Result := sdgExport.FileName
  else
    Result := '';
end;

procedure TMainForm.ProcessCommandline;
var
  DefaultDbFile: string;
  PassedDbFile: string;
  //Conf: TJSONConfig;
begin
  DefaultDbFile := GetAppConfigDir(False) + 'defdb.ccq';
  FDbFileName := DefaultDbFile;
  FDbDefault := True;
  if ParamCount > 0 then
  begin
    PassedDbFile := ParamStr(1);
    if FileExists(PassedDbFile) then
      FDbFileName := PassedDbFile;
  end;

end;

procedure TMainForm.SavetoFile;
var
  Conf: TJSONConfig;
  //Count: integer;
begin
  //Conf.SetValue(TIMER_TITLE, );
  Conf := TJSONConfig.Create(nil);
  Conf.Formatted:=true;
  FClocks.SaveClocks(Conf);
  Conf.Filename := FDbFileName;
  //Conf.Flush;
  Conf.Free;
end;

procedure TMainForm.LoadfromFile;
var
  Conf: TJSONConfig;
  TotalCount, Count: integer;
  NewTimerClock: TTimerClock;
  Hours, Mins, Secs: word;
  Order: TIdList;
  OrderString: TStringList;
  Pos: string;
begin
  Order := nil;
  OrderString := nil;

  if FileExists(FDbFileName) then
  begin

    Conf := TJSONConfig.Create(nil);
    Conf.FileName := FDbFileName;
    //FClocks.LoadClocks(Conf);
    TotalCount := Conf.GetValue(TIMER_CONF_COUNT, 0);
    for Count := 0 to TotalCount - 1 do
    begin
      NewTimerClock := FClocks.AddTimer;
      NewTimerClock.Widget.Caption :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_TITLE, 'Countdown timer');
      Hours := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_HOURS, 0);
      Mins := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_MINUTES, 0);
      Secs := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_SECONDS, 0);
      //TODO: Remove hardcoding
      NewTimerClock.Widget.Duration :=
        EncodeDateTime(2000, 1, 1, Hours, Mins, Secs, 0);
      NewTimerClock.Notifier :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_NOTIFIER, False);
      //NewTimerClock.AddSubscription(FFormWidget);
      //NewTimerClock.Widget.OnSelect:=@ClockSelected;
      PostTimerCreation(NewTimerClock);
    end;
    Order := TIdList.Create;
    OrderString := TStringList.Create;

    if not Conf.GetValue(TIMER_CONF_ORDER, OrderString, '0') then
      ShowMessage('Getting order failed');

    for Pos in OrderString do
    begin
      Order.Add(StrToInt(Pos));
    end;

    FClocks.Widget.SetOrder(Order);

    Conf.Free;
    OrderString.Free;
    Order.Free;
    //FClocks.Widget.Reorder;
  end;
end;


end.
