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
  observers, settings, optionsform, aboutform, BGRABitmap,
  BGRABitmapTypes, FPimage, timeralertform, dateutils, clocks, jsonConf,
  timerframe, fgl, sequence, editform;

const
  FORM_MIN_SIZE = 600;
  TICON_RED_INDEX: integer = 1;
  TICON_GREEN_INDEX: integer = 2;
  TRAY_PROGRESS_ICON_COUNT = 24;

  TRAY_OUTLINE_COLOUR = $3333f2; //$5A9E60;
  TRAY_BG_COLOUR = $c6c6f5; //$DAEADB;
  TRAY_CENTRE_COLOUR = TRAY_BG_COLOUR;
  PROGRESS_COLOUR = $7a2a41; //1A1AB0;

  PROGRESS_COMPLETED = 2.0;

  TRAY_OUTLINE_INSET = 2;
  TRAY_CENTRE_INNER_RADIUS = 2;
  TRAY_CENTRE_OUTER_RADIUS = 4;
  TRAY_BASE_WIDTH = 16;

  RAD_MULTIPLIER = 16;

  APP_ICON_SIZE = 256;

  LAST_TRAY_ICON_DEFAULT = -1;

  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  TIMER_CONF_HOURS = 'hours';
  TIMER_CONF_MINUTES = 'minutes';
  TIMER_CONF_SECONDS = 'seconds';
  TIMER_CONF_NOTIFIER = 'notifier';

  TIMER_CONF_MODALALERT = 'modal_alert';
  TIMER_CONF_TRAYNOTIFiCATION = 'tray_notification';

  //TIMER_CONF_ID = 'id';
  TIMER_CONF_COUNT = 'count';
  TIMER_CONF_ORDER = 'order';

type
  TTimerFrameList = specialize TFPGMap<longword, TfraTimer>;
  TIdList = specialize TFPGList<longword>;

  { TMainForm }

  TMainForm = class(TForm, ITimerObserver)
    aiMoveDown: TAction;
    aiMoveUp: TAction;
    aiDeleteTimer: TAction;
    aiExport: TAction;
    aiNewAlarm: TAction;
    aiOptions: TAction;
    aiQuit: TAction;
    aiAbout: TAction;
    alMain: TActionList;
    aiNewTimer: TAction;
    bbDelete: TBitBtn;
    bbMoveUp: TBitBtn;
    bbMoveDown: TBitBtn;
    hdrTimers: THeaderControl;
    ilMain: TImageList;
    ilMainSmall: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    pmiQuit: TMenuItem;
    pmiShowWindow: TMenuItem;
    pmiExit: TMenuItem;
    MenuItem8: TMenuItem;
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
    pmTray: TPopupMenu;
    sdgExport: TSaveDialog;
    sbxClocks: TScrollBox;
    stbMain: TStatusBar;
    tlbMain: TToolBar;
    ToolButton1: TToolButton;
    tiMain: TTrayIcon;
    tbProgressAuto: TToolButton;
    tbDelete: TToolButton;
    tbMoveUP: TToolButton;
    tbMoveDown: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure aiDeleteTimerExecute(Sender: TObject);
    procedure aiExportExecute(Sender: TObject);
    procedure aiMoveDownExecute(Sender: TObject);
    procedure aiMoveUpExecute(Sender: TObject);
    procedure aiNewAlarmExecute(Sender: TObject);
    procedure aiAboutExecute(Sender: TObject);
    procedure aiNewTimerExecute(Sender: TObject);
    procedure aiOptionsExecute(Sender: TObject);
    procedure aiQuitExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmiShowWindowClick(Sender: TObject);
    procedure tbProgressAutoClick(Sender: TObject);
    //procedure tbShowModalAlertClick(Sender: TObject);
    //procedure tbShowTrayAlertClick(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    procedure tiMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  private
    { private declarations }
    FTrayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FAppProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FTrayStoppedBitmap, FAppStoppedBitmap: TIcon;
    FLastTrayIconIndex: integer;
    //FClockWidget: TClocksWidget;
    //FClocks: TClocks;
    FDbFileName: string;
    FDbDefault: boolean;
    //function GetClocks: TClocksWidget;

    FTimerFrames: TTimerFrameList;
    FOrder: TIdList;
    FCounterClockID: TSequence;

    procedure DrawBaseIconBackground(Bmp: TBGRABitmap);
    procedure DrawBaseIconForeground(Bmp: TBGRABitmap);
    //procedure SetDeleteEnabled(AValue: boolean);
    //procedure SetForm(AValue: TMainForm);
    //procedure SetMoveDownEnabled(AValue: boolean);
    //procedure SetMoveUpEnabled(AValue: boolean);
    //procedure FormShow(Sender: TObject);
    procedure UpdateAlertFormSettings;
    //procedure ExportToFile(Sender: TObject);
    procedure PostTimerCreation(AValue: TfraTimer);
    procedure SetListButtonsStatus;
    procedure ResetHeaderSections;

    function GetAnySelected: boolean;
    //procedure SetWidget(AValue: TClocksWidget);
    function GetCanselectedMoveDown: boolean;
    function GetCanSelectedMoveUp: boolean;
    procedure Reorder;

  public
    { public declarations }
    OnNewTimer: TNotifyEvent;
    OnNewAlarm: TNotifyEvent;
    OnClockDelete: TNotifyEvent;
    OnClockMoveUp: TNotifyEvent;
    OnClockMoveDown: TNotifyEvent;
    OnEXport: TNotifyEvent;
    //property Clocks: TClocksWidget read FClockWidget;
    //procedure NewTimerAdded(Sender: TObject);
    //procedure NewAlarmAdded(Sender: TObject);
    //procedure ClockDeleted(Sender: TObject);
    //procedure ClockMovedUp(Sender: TObject);
    //procedure ClockMovedDown(Sender: TObject);
    procedure ClockSelected(Sender: TObject);
    procedure TimerFinished(Id: integer);
    procedure ProgressUpdate(Progress: single);
    //procedure OptionsEdit(Sender: TObject);
    //procedure ExportClicked(Sender: TObject);
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

    function AddTimer(): TfraTimer;
    //procedure NotifyChange(Sender: TObject);
    procedure HandleTimerFrameIconProgressChange(Sender: TObject);
    procedure SaveClocks(Conf: TJsonConfig);
    procedure DeleteSelected;
    //property Widget: TClocksWidget read FClocksWidget write SetWidget;
    property AnySelected: boolean read GetAnySelected;
    procedure RemoveTimer(IdNew: longword);
    procedure MoveSelectedClocksUp;
    procedure MoveSelectedClocksDown;
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
  FOrder := TIdList.Create;
  Constraints.MinWidth := FORM_MIN_SIZE;
  ;

  FTimerFrames := TTimerFrameList.Create;
  //FActiveTimers := TListTimerClockWidgets.Create;

  FCounterClockID := TSequence.Create;

  OnNewTimer := nil;
  OnEXport := nil;
  FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;

  //FClocks := TClocks.Create;
  FDbDefault := True;

  //DoubleBuffered:=True;
  //FClockWidget := TClocksWidget.Create;
  //FClocks.ScrollBox := sbxClocks;

  //FClocks.Widget := FClockWidget;

  ilMainSmall.GetIcon(TICON_GREEN_INDEX, tiMain.Icon);
  tiMain.Visible := True;

  with GlobalUserConfig do
  begin
    Top := LastPos.Top;
    Left := LastPos.Left;
    Width := LastPos.Width;
    Height := LastPos.Height;

    //tbShowModalAlert.Down := ShowModalAlert;
    //tbShowTrayAlert.Down := ShowTrayAlert;

    tbProgressAuto.Down := AutoProgress;
  end;


  //Self.AlphaBlend:=True;
  //Self.AlphaBlendValue:=50;
  //aiNewTimer.OnExecute := @NewTimerAdded;
  //aiNewAlarm.OnExecute := @NewAlarmAdded;
  //aiOptions.OnExecute := @OptionsEdit;
  {TODO: Add actions for delete, clock up and down}
  //sbDelete.OnClick := @ClockDeleted;
  //sbMoveClockUp.OnClick := @ClockMovedUp;
  //sbMoveClockDown.OnClick := @ClockMovedDown;
  //aiExport.OnExecute := @ExportClicked;
  //OnShow := @FormShow;

  //sbDelete.Caption := '';
  //sbMoveClockDown.Caption := '';
  //sbMoveClockUp.Caption := '';
  bbDelete.Caption := '';
  bbMoveUp.Caption := '';
  bbMoveDown.Caption := '';

  bbDelete.Enabled := False;
  bbMoveUp.Enabled := False;
  bbMoveDown.Enabled := False;

  tbDelete.Enabled := False;
  tbMoveUp.Enabled := False;
  tbMoveDown.Enabled := False;


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
var
  Added: TfraTimer;
  //Proceed: boolean;
begin
  {*if OnNewTimer <> nil then
    OnNewTimer(Sender);*}

  with GlobalUserConfig do
  begin
    //dtpSet.Time := EncodeTime(DefaultTimerHours, DefaultTimerMins,
    //  DefaultTimerSecs, 0);
    frmEditTimer.Duration:=EncodeTime(DefaultTimerHours, DefaultTimerMins,
      DefaultTimerSecs, 0);
    frmEditTimer.Description := DefaultTimerTitle;
    {TODO: This should be read from config}
    frmEditTimer.TrayNotification:=ShowTrayAlert;
    frmEditTimer.ModalAlert:=ShowModalAlert;
  end;

  if frmEditTimer.ShowForAdd then
  begin
    Added := AddTimer;
    Added.Caption:=frmEditTimer.Description;
    Added.dtpSet.Time:= frmEditTimer.Duration;
    Added.ModalAlert:=frmEditTimer.ModalAlert;
    Added.TrayNotification:=frmEditTimer.TrayNotification;
    PostTimerCreation(Added);
  end;
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
    //ShowModalAlert := tbShowModalAlert.Down;
    //ShowTrayAlert := tbShowTrayAlert.Down;
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

procedure TMainForm.aiExportExecute(Sender: TObject);
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
    SaveClocks(Conf);
    Conf.Filename := FileName;
    Conf.Free;
  end;
end;

procedure TMainForm.aiMoveDownExecute(Sender: TObject);
begin
  MoveSelectedClocksDown;
  SetListButtonsStatus;
end;

procedure TMainForm.aiMoveUpExecute(Sender: TObject);
begin
  MoveSelectedClocksUp;
  SetListButtonsStatus;
end;

procedure TMainForm.aiDeleteTimerExecute(Sender: TObject);
begin
  DeleteSelected;
  SetListButtonsStatus;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  Count: integer;
  I: integer;
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
  //FClocks.Destroy;

  for I := 0 to FTimerFrames.Count - 1 do
  begin
    FTimerFrames.Data[i].Free;
  end;

  FCounterClockID.Free;

  //FActiveTimers.Free;
  FTimerFrames.Free;

  FOrder.Free;
  //FClockWidget.Free;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  ResetHeaderSections;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateAlertFormSettings;
end;

procedure TMainForm.pmiShowWindowClick(Sender: TObject);
begin
  if Self.WindowState = wsMinimized then
    Self.WindowState := wsNormal;
  Self.Show;
end;

procedure TMainForm.tbProgressAutoClick(Sender: TObject);
begin
  GlobalUserConfig.AutoProgress := tbProgressAuto.Down;
end;

{procedure TMainForm.tbShowModalAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowModalAlert := tbShowModalAlert.Down;
end;}

{procedure TMainForm.tbShowTrayAlertClick(Sender: TObject);
begin
  GlobalUserConfig.ShowTrayAlert := tbShowTrayAlert.Down;
end;}

procedure TMainForm.tiMainClick(Sender: TObject);
begin
  ShowMessage('Hehe');
end;

procedure TMainForm.tiMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
    pmTray.PopUp;
end;

{*function TMainForm.GetClocks: TClocksWidget;
begin
  Result := FClockWidget;
end;*}

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

{8procedure TMainForm.FormShow(Sender: TObject);
begin
  //frmOptions.OnClose := @OptionsFormClosed;
  UpdateAlertFormSettings;
end;*}

procedure TMainForm.UpdateAlertFormSettings;
begin
  with GlobalUserConfig do
  begin
    frmTimerAlert.Color := ModalBackgroundColour;
    frmTimerAlert.stxMessage.Font.Color := ModalCaptionColour;
    frmTimerAlert.stxAdditional.Font.Color := ModalSubtextColour;
  end;
end;

{*procedure TMainForm.ExportToFile(Sender: TObject);
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
end;*}

procedure TMainForm.PostTimerCreation(AValue: TfraTimer);
begin
  AValue.AddSubscription(Self);
  AValue.OnSelect := @ClockSelected;
  ResetHeaderSections;
end;

procedure TMainForm.SetListButtonsStatus;
begin
  //sbDelete.Enabled := AnySelected;
  bbDelete.Enabled := AnySelected;
  tbDelete.Enabled := AnySelected;
  //sbMoveClockDown.Enabled := GetCanSelectedMoveDown;
  //sbMoveClockUp.Enabled := getCanSelectedMoveUp;
  bbMoveUp.Enabled := getCanSelectedMoveUp;
  bbMoveDown.Enabled := GetCanselectedMoveDown;
  tbMoveUp.Enabled := getCanSelectedMoveUp;
  tbMoveDown.Enabled := GetCanselectedMoveDown;
end;

procedure TMainForm.ResetHeaderSections;
var
  //Ids: TIdList;
  Id: longword;
  Timer: TfraTimer;
  Filled: integer = 0;
  Temp: integer;
begin
  //Ids:=TIdList.Create;
  //GetAllIdS(Ids);
  if FOrder.Count > 0 then
  begin
    Id := FOrder.Items[0];
    Timer := FTimerFrames.KeyData[Id];
    // Find the
    hdrTimers.Sections.Items[0].Width := Timer.cbSelect.Left + Timer.cbSelect.Width;
    Inc(Filled, hdrTimers.Sections.Items[0].Width);
    Temp := Timer.dtpSet.Left + Timer.dtpSet.Width;
    Temp := Temp + ((Timer.bbPlay.Left - Temp) div 2) - Filled;
    hdrTimers.Sections.Items[1].Width := Temp;
    Inc(Filled, hdrTimers.Sections.Items[1].Width);

    Temp := Timer.bbAdjust.Left + Timer.bbAdjust.Width;
    Temp := Temp + ((Timer.lblCountdown.Left - Temp) div 2) - Filled;
    hdrTimers.Sections.Items[2].Width := Temp;
    Inc(Filled, Temp);

    Temp := Timer.lblCountdown.Left + Timer.lblCountdown.Width;
    Temp := Temp + ((Timer.ckbIconProgress.Left - Temp) div 2) - Filled;
    hdrTimers.Sections.Items[3].Width := Temp;
    Inc(Filled, Temp);

    Temp := Timer.ckbIconProgress.Left + Timer.ckbIconProgress.Width;
    Temp := Temp + ((Timer.bbEdit.Left - Temp) div 2) - Filled;
    hdrTimers.Sections.Items[4].Width := Temp;
    Inc(Filled, Temp);

    Temp := sbxClocks.Width - Filled;
    hdrTimers.Sections.Items[5].Width := Temp;
    Inc(Filled, Temp);
  end
  else
  begin
    Filled := 0;
  end;
  //Ids.Free;
end;

function TMainForm.GetAnySelected: boolean;
var
  Count: integer;
  Clock: TfraTimer;
  //TimerClock: TTimerClock;
begin
  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    Clock := FTimerFrames.Data[Count];
    if Clock = nil then
      ShowMessage('Clock is Nil');
    if Clock.Selected then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TMainForm.GetCanselectedMoveDown: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredSelected: boolean;
begin
  EncounteredSelected := False;
  //EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if not FTimerFrames.KeyData[Id].Selected then
    begin
      if EncounteredSelected then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if not EncounteredSelected then
        EncounteredSelected := True;
    end;
  end;
  Result := False;
end;

function TMainForm.GetCanSelectedMoveUp: boolean;
var
  Id: longword;
  //Count: integer;
  EncounteredUnselected: boolean;
begin
  //EncounteredSelected := False;
  EncounteredUnselected := False;
  for Id in FOrder do
  begin
    if FTimerFrames.KeyData[Id].Selected then
    begin
      if EncounteredUnselected then
      begin
        Result := True;
        Exit;
      end;
    end
    else
    begin
      if not EncounteredUnselected then
        EncounteredUnselected := True;
    end;
  end;
  Result := False;
end;


procedure TMainForm.Reorder;
var
  Id: longword;
  //Index: integer;
  TimerWidget: TfraTimer;
  Filled: integer;
  CountTabOrder: integer;
begin
  //Exit;
  {TODO: Remove the hard coding of the header hight}
  Filled := 24;
  //FScrollBox.AutoScroll:=False;
  //FScrollBox.Visible:=False;
  sbxClocks.Height := FOrder.Count * CLOCK_HEIGHT;
  CountTabOrder := 0;
  for Id in FOrder do
  begin
    //Index := FClockWidgets.IndexOf(Id);
    TimerWidget := FTimerFrames.KeyData[Id];
    //FScrollBox.Height:=Filled + Clock.Height;
    //FScrollBox.VertScrollBar.Range:=Filled + Clock.Height;
    TimerWidget.Top := Filled;
    TimerWidget.TabOrder := CountTabOrder;
    // + FScrollBox.VertScrollBar.Size - FScrollBox.VertScrollBar.Position;
    if sbxClocks.VertScrollBar.IsScrollBarVisible then
      TimerWidget.Width := sbxClocks.Width - GetSystemMetrics(SM_CYVSCROLL)
    else
      TimerWidget.Width := sbxClocks.Width;
    //Clock.He;
    Inc(Filled, TimerWidget.Height);
    Inc(CountTabOrder);
    //if Clock.Kind = 'Timer' then
    //begin
    //TimerWidget := TTimerClockWidget(Clock);

    //TimerWidget.FFrame.lblCountdown.Hint := IntToStr(TimerWidget.FFrame.Top);
    //end;

  end;

  //FScrollBox.AutoScroll:=True;
  //FScrollBox.VertScrollBar.Range:=(FOrder.Count) * CLOCK_HEIGHT;

  //FScrollBox.VertScrollBar.Range:=Filled ;
  //FScrollBox.Refresh;
  //FScrollBox.Visible:=True;
  //FScrollBox.Repaint;
  //FScrollBox.ReAlign;

end;

{*procedure TMainForm.NewTimerAdded(Sender: TObject);
var
  Added: TTimerClock;
begin
  {if OnNewTimer <> nil then
    OnNewTimer(Sender);}
  Added := FClocks.AddTimer;
  PostTimerCreation(Added);
end; *}

{*
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
end; *}

{procedure TMainForm.ClockDeleted(Sender: TObject);
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
  DeleteSelected;
  SetListButtonsStatus;
end;}

{procedure TMainForm.ClockMovedUp(Sender: TObject);
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
  MoveSelectedClocksUp;
  SetListButtonsStatus;
end;}

{procedure TMainForm.ClockMovedDown(Sender: TObject);
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
  MoveSelectedClocksDown;
  SetListButtonsStatus;
end;                }

procedure TMainForm.ClockSelected(Sender: TObject);
begin
  SetListButtonsStatus;
end;

procedure TMainForm.TimerFinished(Id: integer);
var
  Hours: word;
  Minutes: word;
  Seconds: word;
  Widget: TfraTimer;
  Duration: TDateTime;
begin
  //DecodeTime(Duration, Hours, Minutes, Seconds, Millis);

  Widget := FTimerFrames.KeyData[Id];
  Duration := Widget.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  if Widget.TrayNotification then
  begin
    tiMain.BalloonHint :=
      Widget.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    tiMain.ShowBalloonHint;
  end;

  if Widget.ModalAlert then
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

{*procedure TMainForm.OptionsEdit(Sender: TObject);
begin
  frmOptions.ShowModal;
end;*}

{*procedure TMainForm.ExportClicked(Sender: TObject);
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
end;*}

procedure TMainForm.OptionsFormClosed();//Sender: TObject; var Action: TCloseAction);
begin

  {Once the options form is closed, tool buttons have to be set according
  to the changed options}
  with GlobalUserConfig do
  begin
    {if tbShowModalAlert.Down <> ShowModalAlert then
    begin
      tbShowModalAlert.Down := ShowModalAlert;
      tbShowModalAlertClick(Self);
    end;

    if tbShowTrayAlert.Down <> ShowTrayAlert then
    begin
      tbShowTrayAlert.Down := ShowTrayAlert;
      tbShowTrayAlertClick(Self);
    end;}

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
  Conf.Formatted := True;
  SaveClocks(Conf);
  Conf.Filename := FDbFileName;
  //Conf.Flush;
  Conf.Free;
end;

procedure TMainForm.LoadfromFile;
var
  Conf: TJSONConfig;
  TotalCount, Count: integer;
  NewTimerClock: TfraTimer;
  Hours, Mins, Secs: word;
  Order: TIdList;
  OrderString: TStringList;
  Pos: string;
  Id: longword;
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
      NewTimerClock := AddTimer;
      NewTimerClock.Caption :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_TITLE, 'Countdown timer');
      Hours := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_HOURS, 0);
      Mins := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_MINUTES, 0);
      Secs := Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_SECONDS, 0);
      //TODO: Remove hardcoding
      NewTimerClock.Duration :=
        EncodeDateTime(2000, 1, 1, Hours, Mins, Secs, 0);
      NewTimerClock.IsProgressOnIcon :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_NOTIFIER, False);

      NewTimerClock.ModalAlert :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_MODALALERT, False);
      NewTimerClock.TrayNotification :=
        Conf.GetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/' + TIMER_CONF_TRAYNOTIFiCATION, False);

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

    Conf.Free;
    OrderString.Free;
    Order.Free;
    Reorder;
  end;
end;

function TMainForm.AddTimer(): TfraTimer;
var
  //NewTimer: TTimerClock;
  Id: longword;
  NewWidget: TfraTimer;
begin
  //NewTimer := TTimerClock.Create;
  //frmEditTimer.ShowModal;
  Id := FCounterClockID.NextVal;
  //NewTimer.Id := Id;

  //NewWidget := FClocksWidget.AddTimer(Id);
  NewWidget := TfraTimer.Create(sbxClocks);
  NewWidget.Id := Id;
  if not GlobalUserConfig.AllowTimerTitleEdit then
  begin
    NewWidget.edtTitle.Color:=clForm;
    NewWidget.edtTitle.ReadOnly:=True;
    //NewWidget.edtTitle.ParentColor:=True;
  end;
  FTimerFrames.Add(Id, NewWidget);
  FOrder.Insert(0, Id);
  Reorder;

  //NewTimer.Widget := NewWidget;
  //NewWidget.OnNotifyChange := @NotifyChange;
  NewWidget.OnProgressOnIconChanged := @HandleTimerFrameIconProgressChange;
  //NewTimer.AddSubscription(NewWidget.);

  //FTimerFrames.Add(Id, NewWidget);
  //NewWidget.Id:=Id;
  {TODO: This section can be cleaned up}
  //NewWidget.OnPlay := @NewWidget.Start;
  //NewWidget.OnStop := @NewWidget.Stop;
  //NewWidget.OnPause := @NewWidget.Pause;
  //NewWidget.OnNotify := @NewWidget.NotifyChange;
  //NewWidget.OnSelect:=@ClockSelected;

  Result := NewWidget;
end;

{procedure TMainForm.NotifyChange(Sender: TObject);
var
  TimerClock: TfraTimer;
  Notifier: TfraTimer;
  Count: integer;
begin
  ShowMessage('Hehe');
  Notifier := TfraTimer(Sender);
  if not Notifier.IsProgressOnIcon then
  begin
    Notifier.PublishProgress(TIMER_PROGRESS_FINISHED);
    Notifier.IsProgressOnIcon := False;
    Exit;
  end;


  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    TimerClock := FTimerFrames.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');
    if TimerClock <> Notifier then
    begin
      TimerClock.IsProgressOnIcon := False;
      if TimerClock.IsProgressOnIcon = True then
      begin
        TimerClock.IsProgressOnIcon := False;
        TimerClock.PublishProgress(TIMER_PROGRESS_FINISHED);
      end;
    end;

  end;

  Notifier.IsProgressOnIcon := True;
  //TODO: Add PublishProgress here
  //Notifier.OnShortTimer(Self);

end;}

// Procedure to handle if
procedure TMainForm.HandleTimerFrameIconProgressChange(Sender: TObject);
var
  SenderTimer: TfraTimer;
  Temp: TfraTimer;
  Count: integer;
begin
  SenderTimer := TfraTimer(Sender);

  if not SenderTimer.IsProgressOnIcon then
  begin
    SenderTimer.PublishProgress(TIMER_PROGRESS_FINISHED);
    //Notifier.IsProgressOnIcon := False;
    Exit;
  end;

  // For all timers other than the sender, uncheck if checked
  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    Temp := FTimerFrames.Data[Count];
    if Temp <> SenderTimer then
      if Temp.IsProgressOnIcon then
      begin
        Temp.CallbackOnProgressOnIconChange := False;
        Temp.IsProgressOnIcon := False;
        Temp.CallbackOnProgressOnIconChange := True;
        Temp.PublishProgress(TIMER_PROGRESS_FINISHED);
      end;
  end;

end;

procedure TMainForm.SaveClocks(Conf: TJsonConfig);
var
  TimerClock: TfraTimer;
  Count: integer;
  OrderStrings: TStringList;
  Order: TIdList;
  Id: longword;
  Index: integer;
begin
  Order := TIdList.Create;
  OrderStrings := TStringList.Create;

  //GetOrder(Order);
  for Id in FOrder do
  begin
    Order.Add(Id);
  end;

  Conf.SetValue(TIMER_CONF_COUNT, FTimerFrames.Count);
  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    TimerClock := FTimerFrames.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_TITLE, TimerClock.Caption);
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_HOURS, HourOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_MINUTES, MinuteOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_SECONDS, SecondOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_NOTIFIER, TimerClock.IsProgressOnIcon);
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_MODALALERT, TimerClock.ModalAlert);
    Conf.SetValue(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/' + TIMER_CONF_TRAYNOTIFiCATION, TimerClock.TrayNotification);
      { While saving, existing IDs of clocks are ignored. They are saved in the
      order they are in the list. The position in the list becomes the new ID.
      The saved order has actual IDs (TimerClock.Id). These have to be
      translated to the new IDs (Count + 1)}

    Index := Order.IndexOf(TimerClock.Id);
    Assert(Index >= 0);
    Order.Items[Index] := Count + 1;

  end;




  for Id in Order do
    OrderStrings.Add(IntToStr(Id));

  Conf.SetValue(TIMER_CONF_ORDER, OrderStrings);
  //ShowMessage('After Savetofile ' + IntToStr(FOrder.Count));
  OrderStrings.Free;
  Order.Free;
end;

procedure TMainForm.DeleteSelected;
var
  Id: longword;
  TimerClock: TfraTimer;
  Count: integer;
  //Index: integer;
  IdList: TIdList;
begin
  IdList := TIdList.Create;

  { FPGMap does not have an iterator. So removal of elements have to be done
  with kid-gloves. A seperate list is created to hold IDs of elements to be
  removed from the list. After iteration, removal is done in a separte loop}

  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    TimerClock := FTimerFrames.Data[Count];
    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    if TimerClock.Running then
      TimerClock.Stop(Self);

    if TimerClock.Selected then
    begin
      //RemoveTimer(TimerClock.Id);
      IdList.Add(TimerClock.Id);
      //TimerClock.Free;
    end;

  end;

  for Id in IdList do
  begin
    RemoveTimer(Id);
    //FTimerFrames.Remove(Id);
    //TimerClock.Free;
  end;
  IdList.Free;

end;

procedure TMainForm.RemoveTimer(IdNew: longword);
var
  RemovedTimer: TfraTimer;
  Index: integer;
begin
  Index := FTimerFrames.IndexOf(IdNew);
  RemovedTimer := TfraTimer(FTimerFrames.Data[Index]);
  FTimerFrames.Remove(IdNew);
  FOrder.Remove(IdNew);
  RemovedTimer.Free;
  Reorder;
end;

procedure TMainForm.MoveSelectedClocksUp;
var
  Id: longword;
  Count: integer;
begin
  Count := 0;
  for Id in FOrder do
  begin
    if Count = 0 then
    begin
      Inc(Count);
      Continue;
    end;

    if FTimerFrames.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FTimerFrames.KeyData[FOrder.Items[Count - 1]].Selected and
        FTimerFrames.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count - 1, Count);
    end;
    Inc(Count);
  end;
  Reorder;
end;

procedure TMainForm.MoveSelectedClocksDown;
var
  Id: longword;
  Count: integer;
  First: boolean;
begin
  First := True;
  for Count := (FTimerFrames.Count - 1) downto 0 do
  begin
    if First then
    begin
      First := False;
      Continue;
    end;
    Id := FOrder.Items[Count];
    if FTimerFrames.KeyData[Id].Selected then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      if not (FTimerFrames.KeyData[FOrder.Items[Count + 1]].Selected and
        FTimerFrames.KeyData[FOrder.Items[Count]].Selected) then
        FOrder.Exchange(Count + 1, Count);
    end;

  end;
  Reorder;
end;


end.
