{

Copyright (C) 2019 Vipin Cherian

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
  settings, optionsform, aboutform, BGRABitmap,
  BGRABitmapTypes, FPimage, timeralertform, dateutils, jsonConf,
  timerframe, fgl, sequence, editform, Math, LazLogger, LMessages,
  {$IF defined(windows) }
  ShlObj, comobj, Win32Int, InterfaceBase,
  {$ENDIF}
  {portaudio, sndfile,} ctypes, audio;

const
  FORM_MIN_WIDTH = 600;
  FORM_MIN_HEIGHT = 300;
  TICON_RED_INDEX: integer = 1;
  TICON_GREEN_INDEX: integer = 2;
  TRAY_PROGRESS_ICON_COUNT = 24;

  //TRAY_OUTLINE_COLOUR = $3333f2; //$5A9E60;
  //TRAY_BG_COLOUR = $c6c6f5; //$DAEADB;
  //TRAY_CENTRE_COLOUR = TRAY_BG_COLOUR;
  PROGRESS_COLOUR = $4352E2;

  PROGRESS_COMPLETED = 2.0;

  TRAY_OUTLINE_INSET = 2;
  TRAY_CENTRE_INNER_RADIUS = 2;
  TRAY_CENTRE_OUTER_RADIUS = 4;
  TRAY_BASE_WIDTH = 16;

  WIDGET_ICON_WIDTH = 24;

  RAD_MULTIPLIER = 16;

  APP_ICON_SIZE = 256;

  LAST_TRAY_ICON_DEFAULT = -1;

  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;
  TIMER_PROGRESS_OFFTRAY: single = 3.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  //TIMER_CONF_HOURS = 'hours';
  //TIMER_CONF_MINUTES = 'minutes';
  //TIMER_CONF_SECONDS = 'seconds';
  TIMER_CONF_DURATION = 'duration';
  TIMER_CONF_NOTIFIER = 'notifier';

  TIMER_CONF_AUDIOFILE = 'audio_file';
  TIMER_CONF_AUDIOLENGTH = 'audio_duration';
  TIMER_CONF_AUDIOLOOP = 'audio_loop';

  TIMER_CONF_MODALALERT = 'modal_alert';
  TIMER_CONF_TRAYNOTIFiCATION = 'tray_notification';

  //TIMER_CONF_ID = 'id';
  TIMER_CONF_COUNT = 'count';
  TIMER_CONF_ORDER = 'order';

  //WM_USER = $400;
  UM_AFTERSHOW = LM_USER;

  PANEL_TIMERCOUNT = 0;
  PANEL_AUDIO = 1;
  PANEL_MESSAGE = 2;

type
  TTimerFrameMap = specialize TFPGMap<longword, TfraTimer>;
  TTimerFrameList = specialize TFPGList<TfraTimer>;
  TIdList = specialize TFPGList<longword>;

  { TfrmMain }

  TfrmMain = class(TForm)
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmiShowWindowClick(Sender: TObject);
    procedure sbxClocksResize(Sender: TObject);
    procedure tbProgressAutoClick(Sender: TObject);
    //procedure tbShowModalAlertClick(Sender: TObject);
    //procedure tbShowTrayAlertClick(Sender: TObject);
    procedure tiMainClick(Sender: TObject);
    procedure tiMainDblClick(Sender: TObject);
    procedure tiMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  private
    { private declarations }
    //TimerCriticalSection: TRTLCriticalSection;
    FTrayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FAppProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FWidgetProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FTrayStoppedBitmap, FAppStoppedBitmap, FWidgetStoppedBitmap: TIcon;
    FLastTrayIconIndex: integer;
    FLastTrayPercent: integer;

    {$IF defined(windows) }
    AppHandle: THandle;
    FTaskBarList: ITaskbarList3;
    {$ENDIF}

    //FClockWidget: TClocksWidget;
    //FClocks: TClocks;
    FDbFileName: string;
    FDbDefault: boolean;
    //function GetClocks: TClocksWidget;

    FTimerFrames: TTimerFrameMap;
    FActiveTimerFrames: TTimerFrameList;
    FOrder: TIdList;
    FCounterClockID: TSequence;

    FShortTimer: TTimer;

    { This is an invisible timer frame, the purpose of which
    is to aid in resizing header sections when there are no timers
    added. It is a waste of space, and altogether inelegant,
    but I could not find a better and reliable way to achieve
    the same functionality.}
    FReference: TfraTimer;

    //FAudioWorking: boolean;

    procedure CreateBitmaps;
    function GetStatusMessage: string;
    procedure PostTimerCreation(AValue: TfraTimer);
    procedure SetListButtonsStatus;
    procedure ResizeHeaderSections;

    function GetAnySelected: boolean;
    //procedure SetWidget(AValue: TClocksWidget);
    function GetCanselectedMoveDown: boolean;
    function GetCanSelectedMoveUp: boolean;
    procedure Reorder;
    procedure SetStatusMessage(AValue: string);
    procedure ShowInForeground;
    procedure UpdateStatusTimerCount;

  public
    { public declarations }
    //OnNewTimer: TNotifyEvent;
    //OnNewAlarm: TNotifyEvent;
    //OnClockDelete: TNotifyEvent;
    //OnClockMoveUp: TNotifyEvent;
    //OnClockMoveDown: TNotifyEvent;
    //OnEXport: TNotifyEvent;

    procedure ClockSelected(Sender: TfraTimer);
    procedure TimerFinished(Sender: TfraTimer; UserInitiated: boolean);
    procedure TimerPaused(Sender: TfraTimer);
    procedure TimerStarted(Sender: TfraTimer);
    procedure ProgressUpdate(Widget: TfraTimer; Progress: single);
    //procedure TimerProgressUpdated(Sender: TObject);

    procedure OptionsFormClosed();
    function GetExportFileName: string;

    procedure ProcessCommandline;
    procedure SavetoFile;
    procedure LoadfromFile;
    //property Clocks: TClocks read FClocks;

    function AddTimer(): TfraTimer;
    //procedure NotifyChange(Sender: TObject);
    procedure HandleTimerFrameIconProgressChange(Sender: TfraTimer);
    procedure SaveClocks(Conf: TJsonConfig);
    procedure DeleteSelected;
    //property Widget: TClocksWidget read FClocksWidget write SetWidget;
    property AnySelected: boolean read GetAnySelected;
    procedure RemoveTimer(IdNew: longword);
    procedure MoveSelectedClocksUp;
    procedure MoveSelectedClocksDown;
    procedure OnShortTimer(Sender: TObject);
    procedure AfterShow(var Msg: TLMessage); message UM_AFTERSHOW;
    procedure AfterShow2(Data: PtrInt);
    procedure ShowModalAlert(Data: PtrInt);
    property StatusMessage: string read GetStatusMessage write SetStatusMessage;
    //property AudioWorking: boolean read FAudioWorking;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
//var
//PaErrCode: PaError;
{$IFNDEF AUDIO_STATIC}
//Status: boolean;
{$ENDIF}
//DefaultDevice: integer;
//DeviceInfo: PPaDeviceInfo;
begin
  //InitCriticalSection(TimerCriticalSection);

  { Obtain the application handle, and the taskbar COM object.
  This has to be done at the beginning, as calls are made to progressupdate
  from within FormCreate. This cannot be pushed down }

  {$IF defined(windows) }
  AppHandle := TWin32WidgetSet(WidgetSet).AppHandle;
  FTaskBarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList3;
  {$ENDIF}

  FOrder := TIdList.Create;
  Constraints.MinWidth := FORM_MIN_WIDTH;
  Constraints.MinHeight := FORM_MIN_HEIGHT;
  ;

  FTimerFrames := TTimerFrameMap.Create;
  FActiveTimerFrames := TTimerFrameList.Create;
  //FActiveTimers := TListTimerClockWidgets.Create;

  FCounterClockID := TSequence.Create;

  //OnNewTimer := nil;
  //OnEXport := nil;
  FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
  FLastTrayPercent := 0;

  //FClocks := TClocks.Create;
  FDbDefault := True;

  //DoubleBuffered:=True;
  //FClockWidget := TClocksWidget.Create;
  //FClocks.ScrollBox := sbxClocks;

  //FClocks.Widget := FClockWidget;

  ilMainSmall.GetIcon(TICON_GREEN_INDEX, tiMain.Icon);
  tiMain.Visible := True;

  bbDelete.Caption := '';
  bbMoveUp.Caption := '';
  bbMoveDown.Caption := '';

  bbDelete.Enabled := False;
  bbMoveUp.Enabled := False;
  bbMoveDown.Enabled := False;

  tbDelete.Enabled := False;
  tbMoveUp.Enabled := False;
  tbMoveDown.Enabled := False;

  bbDelete.DoubleBuffered := True;
  bbMoveUp.DoubleBuffered := True;
  bbMoveDown.DoubleBuffered := True;
  DoubleBuffered := True;

  stbMain.DoubleBuffered := True;

  FShortTimer := TTimer.Create(nil);
  FShortTimer.Interval := 200;
  FShortTimer.Enabled := False;
  FShortTimer.OnTimer := @OnShortTimer;

  FReference := TfraTimer.Create(nil);
  FReference.Visible := False;
  FReference.Align := alNone;
  FReference.Anchors := [];

  CreateBitmaps;


  //FAudioWorking := TAudio.Loaded;
  {
  {$IFNDEF AUDIO_STATIC}
  FAudioWorking := Pa_Load(LIB_PORTAUDIO);
  if not FAudioWorking then
  begin
    DebugLn('Could not load portaudio');
    //ReadKey;
    //exit;
  end;

  //FAudioWorking:=Status;

  { Load sndfile library only if portaudio was loaded successfully }

  if FAudioWorking then
  begin
    FAudioWorking := sf_load(LIB_SNDFILE);
    if not FAudioWorking then
    begin
      DebugLn('Could not load sndfile');
      //ReadKey;
      //exit;
      Pa_Unload();
    end;
  end;

  //FAudioWorking:=Status;

{$ENDIF}

  if FAudioWorking then
  begin
    PaErrCode := Pa_Initialize();
    if PaErrCode <> cint(paNoError) then
    begin
      DebugLn('Error in Pa_Initialize()');

      FAudioWorking := False;

      { If portaudio cannot be initialised, then audio will not work.
      Unload libraries }
      {$IFNDEF AUDIO_STATIC}
      sf_Unload();
      Pa_Unload();
      {$ENDIF}
    end;
  end;
  }
  // Check if default audio device has changed
  {if FAudioWorking then
  begin
    //DefaultDevice:=Pa_GetDefaultOutputDevice();
    if GlobalUserConfig.AudioDevice >= 0 then
    begin
      DeviceInfo := Pa_GetDeviceInfo(GlobalUserConfig.AudioDevice);
      if DeviceInfo = nil then
      begin
        DebugLn('Audio device stored in confi file does not exists. Device #' +
          IntToStr(GlobalUserConfig.AudioDevice));
        ShowMessage('Configured audio device not found. Resetting to default');
        GlobalUserConfig.AudioDevice := DEF_AUDIO_DEVICE;
      end
      else
      begin
        if StrPas(DeviceInfo^.Name) <> GlobalUserConfig.AudioDeviceName then
        begin
          DebugLn('Stored audio device name does not match ' +
            'with the current ID. ID - ' + IntToStr(
            GlobalUserConfig.AudioDevice) + ', Stored Device Name - ' +
            GlobalUserConfig.AudioDeviceName + ', Current Device Name - ' +
            StrPas(DeviceInfo^.Name));
          GlobalUserConfig.AudioDevice := DEF_AUDIO_DEVICE;
          ShowMessage('Configured audio device name does not match. ' +
            'Resetting to default');
        end;
      end;
    end;

    if GlobalUserConfig.AudioDevice = DEF_AUDIO_DEVICE then
    begin
      DefaultDevice := Pa_GetDefaultOutputDevice();
      if DefaultDevice = paNoDevice then
      begin
        DebugLn('No default device found. So no audio.');
        FAudioWorking := False;
      end;
    end;

  end;}

  stbMain.BeginUpdate;
  if TAudio.Loaded then
    stbMain.Panels[PANEL_AUDIO].Text := 'Audio: Ok'
  else
    stbMain.Panels[PANEL_AUDIO].Text := 'Audio: Error';
  stbMain.EndUpdate;
end;

procedure TfrmMain.aiNewTimerExecute(Sender: TObject);
var
  Added: TfraTimer;
  //Proceed: boolean;
  TempAudio: TAudio;
begin
  {*if OnNewTimer <> nil then
    OnNewTimer(Sender);*}

  with GlobalUserConfig do
  begin
    //dtpSet.Time := EncodeTime(DefaultTimerHours, DefaultTimerMins,
    //  DefaultTimerSecs, 0);
    frmEdit.Duration := DefaultTimerDuration;
    //EncodeTime(DefaultTimerHours, DefaultTimerMins,
    //  DefaultTimerSecs, 0);
    frmEdit.Description := DefaultTimerTitle;
    frmEdit.TrayNotification := ShowTrayAlert;
    frmEdit.ModalAlert := ShowModalAlert;
    { We are not providing an option to keep audio looped by default }
    frmEdit.ckbLoop.Checked := False;
  end;
  TempAudio := nil;

  if TAudio.Loaded then
  begin
    TempAudio := TAudio.Create;
    frmEdit.Audio := TempAudio;
  end
  else
  begin
    frmEdit.Audio := nil;
    frmEdit.AudioFileName := '';
    frmEdit.AudioDuration := 0;
    frmEdit.AudioLooped := False;
  end;

  if frmEdit.ShowForAdd then
  begin
    Added := AddTimer;
    Added.Caption := frmEdit.Description;
    Added.dtpSet.Time := frmEdit.Duration;
    Added.ModalAlert := frmEdit.ModalAlert;
    Added.TrayNotification := frmEdit.TrayNotification;
    //Added.Audio.Looped := frmEdit.ckbLoop.Checked;
    //Added.Audio := TempAudio;
    if TAudio.Loaded then
    begin
      Added.Audio := TempAudio;
      Added.Audio.Looped := frmEdit.ckbLoop.Checked;
    end
    else
    begin
      Added.AudioInfo.FileName := frmEdit.AudioFileName;
      Added.AudioInfo.Duration := frmEdit.AudioDuration;
      Added.AudioInfo.Looped := frmEdit.ckbLoop.Checked;
    end;
    PostTimerCreation(Added);
  end
  else
    TempAudio.Free;
end;

procedure TfrmMain.aiOptionsExecute(Sender: TObject);
begin
  //ShowMessage('Hehe');
  frmOptions.ShowModal;
  OptionsFormClosed;
end;

procedure TfrmMain.aiQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CurrPosNormal, CurrPosRestored: TRect;
  Count: integer;
  StartTickCount: longword;
begin
  { if any audio is playing, stop }
  if TAudio.Loaded then
  begin
    StatusMessage := 'Stopping sounds being played if any...';
    Cursor := crHourglass;
    for Count := 0 to FTimerFrames.Count - 1 do
    begin
      FTimerFrames.Data[Count].Audio.Abort;

      StartTickCount := GetTickCount64;
      { Abort is asynchronous, wait till each timer aborts.
      Also, we do not wait for more than two seconds per timer.
      After that, it is past caring. Tardiness can be tolerated only as much. }
      while FTimerFrames.Data[Count].Audio.Playing do
      begin
        DebugLn('Waiting for frame ' + IntToStr(Count) + ' to stop audio');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + 2000) then
          break;
      end;
    end;
  end;

  CurrPosNormal.Top := Top;
  CurrPosNormal.Left := Left;
  //CurrPosNormal.Right := CurrPosNormal.Left + Width;
  CurrPosNormal.Width := Width;
  CurrPosNormal.Height := Height;
  //CurrPosNormal.Bottom := CurrPosNormal.Top + Height;

  CurrPosRestored.Top := RestoredTop;
  CurrPosRestored.Left := RestoredLeft;
  CurrPosRestored.Width := RestoredWidth;
  CurrPosRestored.Height := RestoredHeight;

  with GlobalUserConfig do
  begin
    LastPosNormal := CurrPosNormal;
    LastPosRestored := CurrPosRestored;
    //ShowModalAlert := tbShowModalAlert.Down;
    //ShowTrayAlert := tbShowTrayAlert.Down;
    AutoProgress := tbProgressAuto.Down;
    LastWindowState := WindowState;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
{var
  CurrPosNormal, CurrPosRestored: TRect;}
begin
  if GlobalUserConfig.QueryExit then
    CanClose := MessageDlg('Do you really want to close the application?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;

end;

procedure TfrmMain.aiAboutExecute(Sender: TObject);
begin
  //ShowMessage(IntToStr(tiMain.Canvas.Width));
  //ShowMessage('Catbell by Vipin Cherian (c) 2017');
  frmAbout.ShowModal;
  //tiMain.
end;

procedure TfrmMain.aiNewAlarmExecute(Sender: TObject);
begin

end;

procedure TfrmMain.aiExportExecute(Sender: TObject);
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

procedure TfrmMain.aiMoveDownExecute(Sender: TObject);
begin
  MoveSelectedClocksDown;
  SetListButtonsStatus;
end;

procedure TfrmMain.aiMoveUpExecute(Sender: TObject);
begin
  MoveSelectedClocksUp;
  SetListButtonsStatus;
end;

procedure TfrmMain.aiDeleteTimerExecute(Sender: TObject);
begin
  DeleteSelected;
  SetListButtonsStatus;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Count: integer;
  I: integer;
begin
  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin
    FTrayProgressIcons[Count].Free;
    FAppProgressIcons[Count].Free;
    FWidgetProgressIcons[Count].Free;
  end;
  FTrayStoppedBitmap.Free;
  FAppStoppedBitmap.Free;
  FWidgetStoppedBitmap.Free;
  //inherited Destroy;
  SavetoFile;
  //FClocks.Destroy;

  for I := 0 to FTimerFrames.Count - 1 do
  begin
    FTimerFrames.Data[i].Free;
  end;

  FCounterClockID.Free;

  FReference.Free;

  //FActiveTimers.Free;
  FActiveTimerFrames.Free;
  FTimerFrames.Free;

  FOrder.Free;

  FShortTimer.Free;
  {
  if FAudioWorking then
    Pa_Terminate;
{$IFNDEF AUDIO_STATIC}
  Sf_Unload;
  Pa_Unload;
{$ENDIF}
}
  //FClockWidget.Free;
  //DoneCriticalsection(TimerCriticalSection);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  ;//ResizeHeaderSections;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  with GlobalUserConfig do
  begin
    if LastWindowState = wsMaximized then
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(LastPosRestored.Left, LastPosRestored.Top,
        LastPosRestored.Width, LastPosRestored.Height);
      WindowState := wsMaximized;
    end
    else
    begin
      WindowState := wsNormal;
      BoundsRect := Bounds(LastPosNormal.Left, LastPosNormal.Top,
        LastPosNormal.Width, LastPosNormal.Height);
    end;

    {if FTimerFrames.Count = 0 then
      aiNewTimer.Execute;}
    //PostMessage(Handle, UM_AFTERSHOW, 0, 0);
    Application.QueueAsyncCall(@AfterShow2, 0);
    {Top := LastPosNormal.Top;
    Left := LastPosNormal.Left;
    Width := LastPosNormal.Width;
    Height := LastPosNormal.Height;}

    //tbShowModalAlert.Down := ShowModalAlert;
    //tbShowTrayAlert.Down := ShowTrayAlert;

    tbProgressAuto.Down := AutoProgress;

  end;
  //UpdateAlertFormSettings;
end;

procedure TfrmMain.pmiShowWindowClick(Sender: TObject);
begin
  ShowInForeground;
end;

procedure TfrmMain.sbxClocksResize(Sender: TObject);
begin
  ResizeHeaderSections;
end;

procedure TfrmMain.tbProgressAutoClick(Sender: TObject);
begin
  GlobalUserConfig.AutoProgress := tbProgressAuto.Down;
end;

procedure TfrmMain.tiMainClick(Sender: TObject);
begin
  ShowInForeground;
end;

procedure TfrmMain.tiMainDblClick(Sender: TObject);
begin
  ShowInForeground;
end;

procedure TfrmMain.tiMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
    pmTray.PopUp;
end;


procedure TfrmMain.PostTimerCreation(AValue: TfraTimer);
begin
  //AValue.AddSubscription(Self);
  //AValue.OnSelect := @ClockSelected;
  ResizeHeaderSections;
end;

procedure TfrmMain.CreateBitmaps;
var
  FinalBmp, HiresBmp: TBGRABitmap;
  InSet: integer;
  TrayIconSize, AppIconSize: integer;
  Stream: TResourceStream;
  Count: integer;
begin
  TrayIconSize := GetSystemMetrics(SM_CXSMICON);
  AppIconSize := GetSystemMetrics(SM_CXICON);

  // Read the image in resources to a stream
  Stream := TResourceStream.Create(hinstance, '256_HOURGLASS_FLAT', RT_RCDATA);

  { Create one hi-res image and then resize it and assign}
  HiresBmp := TBGRABitmap.Create(Stream);

  FinalBmp := HiresBmp.Resample(TrayIconSize, TrayIconSize, rmFineResample) as
    TBGRABitmap;

  FTrayStoppedBitmap := TIcon.Create;
  FTrayStoppedBitmap.Assign(FinalBmp.Bitmap);

  FinalBmp.Free;

  FinalBmp := HiresBmp.Resample(AppIconSize, AppIconSize, rmFineResample) as
    TBGRABitmap;

  FAppStoppedBitmap := TIcon.Create;
  FAppStoppedBitmap.Assign(FinalBmp.Bitmap);

  FinalBmp.Free;

  FinalBmp := HiresBmp.Resample(WIDGET_ICON_WIDTH, WIDGET_ICON_WIDTH, rmFineResample) as
    TBGRABitmap;

  FWidgetStoppedBitmap := TIcon.Create;
  FWidgetStoppedBitmap.Assign(FinalBmp.Bitmap);

  FinalBmp.Free;

  ProgressUpdate(nil, PROGRESS_COMPLETED);
  HiresBmp.Free;
  Stream.Free;


  Stream := TResourceStream.Create(hInstance, '256_PROGRESS_BASE', RT_RCDATA);
  InSet := 48;

  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin

    //FinalBmp :=
    //TBGRABitmap.Create(TrayIconSize, TrayIconSize, BGRAPixelTransparent);
    Stream.Seek(0, soFromBeginning);
    HiResBmp := TBGRABitmap.Create(Stream);
    //BGRAReplace(FinalBmp, FinalBmp.Resample(TrayIconSize, TrayIconSize,
    //  rmFineResample) as TBGRABitmap);

    with HiResBmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie(Inset, Inset, APP_ICON_SIZE - Inset, APP_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER, -(15 * RAD_MULTIPLIER * (Count - 1)));

      { To give it a glossy feel, we try to add a translucent
      white sheen to the left semi-circle }
      CanvasBGRA.Brush.Opacity := 100;
      CanvasBGRA.Pen.Opacity := 100;
      CanvasBGRA.Brush.Color := clWhite;
      CanvasBGRA.Pen.Color := clWhite;
      CanvasBGRA.Pie(Inset, Inset, APP_ICON_SIZE - Inset, APP_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER,
        { We need to draw only half the circle, or the current pie,
        whichever is lesser. } -(15 * RAD_MULTIPLIER * Max(
        (Count - 1), TRAY_PROGRESS_ICON_COUNT div 2))
        );
    end;
    //DrawBaseIconForeground(FinalBmp);

    FinalBmp := HiresBmp.Resample(TrayIconSize, TrayIconSize, rmFineResample) as
      TBGRABitmap;

    FTrayProgressIcons[Count] := TIcon.Create;
    FTrayProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    FinalBmp := HiresBmp.Resample(AppIconSize, AppIconSize, rmFineResample) as
      TBGRABitmap;

    //DrawBaseIconForeground(FinalBmp);
    FAppProgressIcons[Count] := TIcon.Create;
    FAppProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    FinalBmp := HiresBmp.Resample(WIDGET_ICON_WIDTH, WIDGET_ICON_WIDTH,
      rmFineResample) as TBGRABitmap;

    //DrawBaseIconForeground(FinalBmp);
    FWidgetProgressIcons[Count] := TIcon.Create;
    FWidgetProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    HiresBmp.Free;
  end;
  Stream.Free;
end;

function TfrmMain.GetStatusMessage: string;
begin
  Result := stbMain.Panels[PANEL_MESSAGE].Text;
end;

procedure TfrmMain.SetListButtonsStatus;
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

procedure TfrmMain.ResizeHeaderSections;
var
  //Ids: TIdList;
  Id: longword;
  Timer: TfraTimer;
  Filled: integer = 0;
  Temp: integer;
  ScrollbarWidth: integer = 0;
begin
  //Ids:=TIdList.Create;
  //GetAllIdS(Ids);
  if FOrder.Count > 0 then
  begin
    Id := FOrder.Items[0];
    Timer := FTimerFrames.KeyData[Id];
  end
  else
  begin
    //FReference.Width:=sbxClocks.ClientWidth;
    //FReference.ReAlign;
    //Freference.Refresh;
    //FReference.Repaint;
    Timer := FReference;
  end;
  // Find the
  hdrTimers.Sections.Items[0].Width := Timer.cbSelect.Left + Timer.cbSelect.Width;
  Inc(Filled, hdrTimers.Sections.Items[0].Width);

  Temp := Timer.dtpSet.Left + Timer.dtpSet.Width;
  Inc(Temp, ((Timer.bbPlay.Left - Temp) div 2));

    { If there are no timers added, we are using FReference as a reference.
    It is an invisible frame and is not under the scrollbar hence its width and
    the scrollbar's may not be the same. This needs to be taken into account,
    and an adjustment of (sbxClocks.Width - Timer.Width) needs to be made.}

  Inc(Temp, sbxClocks.ClientWidth - Timer.Width - Filled);
  //Temp := Temp - Filled;
  hdrTimers.Sections.Items[1].Width := Temp;

  Inc(Filled, hdrTimers.Sections.Items[1].Width - (sbxClocks.ClientWidth - Timer.Width));
  //Filled := Filled - (sbxClocks.Width - Timer.Width);

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

  Temp := Timer.Width - Filled;
  hdrTimers.Sections.Items[5].Width := Temp;
  Inc(Filled, Temp);

  //Ids.Free;
end;

function TfrmMain.GetAnySelected: boolean;
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

function TfrmMain.GetCanselectedMoveDown: boolean;
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

function TfrmMain.GetCanSelectedMoveUp: boolean;
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


procedure TfrmMain.Reorder;
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
  //sbxClocks.Height := FOrder.Count * CLOCK_HEIGHT;
  CountTabOrder := 0;
  hdrTimers.Top := 0;
  for Id in FOrder do
  begin
    //Index := FClockWidgets.IndexOf(Id);
    TimerWidget := FTimerFrames.KeyData[Id];
    //FScrollBox.Height:=Filled + Clock.Height;
    //FScrollBox.VertScrollBar.Range:=Filled + Clock.Height;
    TimerWidget.Top := Filled;
    TimerWidget.TabOrder := CountTabOrder;
    // + FScrollBox.VertScrollBar.Size - FScrollBox.VertScrollBar.Position;
    {if sbxClocks.VertScrollBar.IsScrollBarVisible then
      TimerWidget.Width := sbxClocks.Width - GetSystemMetrics(SM_CYVSCROLL)
    else
      TimerWidget.Width := sbxClocks.Width;}
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
  //sbxClocks.Refresh;
  //FScrollBox.Visible:=True;
  //sbxClocks.Repaint;
  //sbxClocks.ReAlign;

end;

procedure TfrmMain.SetStatusMessage(AValue: string);
begin
  stbMain.BeginUpdate;
  if stbMain.Panels[PANEL_MESSAGE].Text <> Avalue then
    stbMain.Panels[PANEL_MESSAGE].Text := Avalue;
  stbMain.EndUpdate;

end;

procedure TfrmMain.ShowInForeground;
begin
  if WindowState = wsMinimized then
    WindowState := wsNormal;
  Show;
end;

procedure TfrmMain.UpdateStatusTimerCount;
var
  TextMessage: string;
begin
  TextMessage := 'Running: ' + IntToStr(FActiveTimerFrames.Count) +
    '/' + IntToStr(FTimerFrames.Count);
  if FShortTimer.Enabled then
    TextMessage := TextMessage + ' +'
  else
    TextMessage := TextMessage + ' -';
  stbMain.Panels[PANEL_TIMERCOUNT].Text := TextMessage;
end;


procedure TfrmMain.ClockSelected(Sender: TfraTimer);
begin
  SetListButtonsStatus;
end;

procedure TfrmMain.TimerFinished(Sender: TfraTimer; UserInitiated: boolean);
var
  Hours: word;
  Minutes: word;
  Seconds: word;
  //Sender: TfraTimer;
  Duration: TDateTime;
  Message: string;
begin

  //Sender := TfraTimer(Sender);
  //Id := Sender.Id;
  Duration := Sender.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  try
    //Index := FActiveTimerFrames.IndexOf(Id);
    //if Index <> -1 then
    //begin
    if FActiveTimerFrames.Remove(Sender) = -1 then
      DebugLn('Unable to remove from active timerframes.');
    ;
    //end;
    FShortTimer.Enabled := (FActiveTimerFrames.Count > 0);

  except
    on E: Exception do
      ShowMessage('Error(1): ' + E.ClassName + #13#10 + E.Message);
  end;

  if Sender.TrayNotification and (not UserInitiated) then
  begin
    tiMain.BalloonHint :=
      Sender.Caption + ' completed. (' + Format('%.2d', [Hours]) +
      ':' + Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    tiMain.ShowBalloonHint;
  end;

  if Sender.ModalAlert and (not UserInitiated) then
  begin
    Message :=
      Sender.Caption + ' (' + Format('%.2d', [Hours]) + ':' +
      Format('%.2d', [Minutes]) + ':' + Format('%.2d', [Seconds]) + ')';
    //frmTimerAlert.stxAdditional.Caption := Message;
    frmAlert.lbMessages.Items.Add(Message);

    Application.QueueAsyncCall(@ShowModalAlert, 0);
  end;
  UpdateStatusTimerCount;
  //DebugLn('Exiting TimerFinished. Timer ID - ' + InttoStr(Id));

end;

procedure TfrmMain.TimerPaused(Sender: TfraTimer);
begin
  //Sender := TfraTimer(Sender);
  try
    //Index := FActiveTimerFrames.IndexOf(Sender.Id);
    //if Index <> -1 then
    //begin
    FActiveTimerFrames.Remove(Sender);
    //end;
    FShortTimer.Enabled := (FActiveTimerFrames.Count > 0);

    {$IF defined(windows)}
    { If progress of this timer is shown, then pause it in taskbar}
    if Sender.IsProgressOnIcon then
    begin
      FTaskBarList.SetProgressState(AppHandle, TBPF_PAUSED);
    end;
    {$ENDIF}
  except
    on E: Exception do
      ShowMessage('Error (2): ' + E.ClassName + #13#10 + E.Message);
  end;

end;

procedure TfrmMain.TimerStarted(Sender: TfraTimer);
var
  Index: integer;
begin
  try
    Sender := TfraTimer(Sender);
    FShortTimer.Enabled := True;
    Index := FActiveTimerFrames.IndexOf(Sender);
    if Index = -1 then
    begin
      FActiveTimerFrames.Add(Sender);
    end;
    UpdateStatusTimerCount;

    {$IF defined(windows)}
    { If progress of this timer is shown, then un-pause it in taskbar}
    if Sender.IsProgressOnIcon then
    begin
      FTaskBarList.SetProgressState(AppHandle, TBPF_NORMAL);
    end;
    {$ENDIF}
  except
    on E: Exception do
      ShowMessage('Error (3): ' + E.ClassName + #13#10 + E.Message);
  end;

end;

procedure TfrmMain.ProgressUpdate(Widget: TfraTimer; Progress: single);
var
  //Bmp: TBitmap;
  Index: integer;
  TaskbarPercent: integer;
begin
  if (Progress > (TIMER_PROGRESS_FINISHED - 0.01)) and
    (Progress < (TIMER_PROGRESS_OFFTRAY + 0.01)) then
  begin
    {If the timer has stopped or has been taken off the tray then
    fix the tray and application icons accordingly.}
    tiMain.Icon.Assign(FTrayStoppedBitmap);
    Icon.Assign(FTrayStoppedBitmap);
    Application.Icon.Assign(FAppStoppedBitmap);

    {$IF defined(windows) }
    FTaskBarList.SetProgressState(AppHandle, TBPF_NOPROGRESS);
    //FTaskBarList.SetProgressValue(AppHandle, 0, 100);
    {$ENDIF}

    FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
    FLastTrayPercent := 0;

    { Change the timer image only if it has really finished.}
    if (Widget <> nil) and (Progress < (TIMER_PROGRESS_FINISHED + 0.01)) then
    begin
      Widget.imgTimer.Picture.Assign(FWidgetStoppedBitmap);
      Widget.LastProgressIconIndex := LAST_TRAY_ICON_DEFAULT;
      //Widget.LastProgressPercent:=0;
    end;
  end
  { Timer has not been stopped, but its progress is not not published
  through the tray}
  {else if (Progress > (TIMER_PROGRESS_OFFTRAY - 0.01)) and
    (Progress < (TIMER_PROGRESS_OFFTRAY + 0.01)) then
  begin
    tiMain.Icon.Assign(FTrayStoppedBitmap);
    Icon.Assign(FTrayStoppedBitmap);
    Application.Icon.Assign(FAppStoppedBitmap);

    {$IF defined(windows) }
    FTaskBarList.SetProgressState(AppHandle, TBPF_NOPROGRESS);
    //FTaskBarList.SetProgressValue(AppHandle, 0, 100);
    {$ENDIF}

    FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
    FLastTrayPercent := 0;
  end }
  else
  begin
    Index := Floor(Progress * 24.0);
    TaskbarPercent := Ceil(Progress * 100);
    //WriteLn('Index is ' + IntToStr(Index));
    if Index >= 24 then
      Index := 23;
    Assert((Index >= 0) and (Index < TRAY_PROGRESS_ICON_COUNT));
    if Widget = nil then
      Exit;
    if Widget.IsProgressOnIcon then
    begin
      {Redraw icons only if there is a change}
      if FLastTrayIconIndex <> Index then
      begin
        tiMain.Icon.Assign(FTrayProgressIcons[Index + 1]);
        //FForm.tiMain.Icon.Handle:=FTrayProgressIcons[Index + 1].Handle;
        Icon.Assign(FTrayProgressIcons[Index + 1]);
        //FForm.Icon.Handle := FTrayProgressIcons[Index + 1].Handle;
        Application.Icon.Assign(FAppProgressIcons[Index + 1]);
        FLastTrayIconIndex := Index;
      end;

      {In Windows, set the progress in task bar}
      {$IF defined(windows) }
      if FLastTrayPercent <> TaskbarPercent then
      begin
        if Widget.Running and (not Widget.Paused) then
          FTaskBarList.SetProgressState(AppHandle, TBPF_Normal)
        else if Widget.Paused then
          FTaskBarList.SetProgressState(AppHandle, TBPF_PAUSED)
        else
          Assert(False); // We should not come here
        FTaskBarList.SetProgressValue(AppHandle, TaskbarPercent, 100);
      end;
      {$ENDIF}
    end;

    { Irrespective of whether progress is shown in tray/application icon,
    for all icons, progress is shown in the image in the timer UI frame }
    if Widget.LastProgressIconIndex <> Index then
    begin
      Widget.imgTimer.Picture.Assign(FWidgetProgressIcons[Index + 1]);
      Widget.LastProgressIconIndex := Index;
    end;

    {if Widget.LastProgressPercent <> TaskbarPercent then
    begin
      Widget.LastProgressPercent := TaskbarPercent;
    end;}

  end;

end;

{procedure TfrmMain.TimerProgressUpdated(Sender: TObject);
var
  TimerFrame: TfraTimer;
begin
  TimerFrame := TfraTimer(Sender);
  ProgressUpdate(TimerFrame.Progress);
end;}

procedure TfrmMain.OptionsFormClosed();//Sender: TObject; var Action: TCloseAction);
var
  Temp: TfraTimer;
  Count: integer;
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
    for Count := 0 to FTimerFrames.Count - 1 do
    begin
      Temp := FTimerFrames.Data[Count];
      Temp.TitleEditable := AllowTimerTitleEdit;
    end;
  end;

  //UpdateAlertFormSettings;
end;

function TfrmMain.GetExportFileName: string;
begin
  if sdgExport.Execute then
    Result := sdgExport.FileName
  else
    Result := '';
end;

procedure TfrmMain.ProcessCommandline;
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

procedure TfrmMain.SavetoFile;
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

procedure TfrmMain.LoadfromFile;
var
  Conf: TJSONConfig;
  TotalCount, Count: integer;
  NewTimerClock: TfraTimer;
  ErrorText: string;
  fs: TFormatSettings;
  //Success: boolean;
  //Order: TIdList;
  //OrderString: TStringList;
  //Pos: string;
  //Id: longword;
  //IdDir: string;
begin
  //Order := nil;
  //OrderString := nil;
  ErrorText := '';

  if FileExists(FDbFileName) then
  begin

    Conf := TJSONConfig.Create(nil);
    Conf.FileName := FDbFileName;
    //FClocks.LoadClocks(Conf);
    TotalCount := Conf.GetValue(TIMER_CONF_COUNT, 0);

    //Keys := TStringList.Create;

    //Keys.Free;

    for Count := 0 to TotalCount - 1 do
    begin
      Conf.OpenKey(UTF8Decode(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/'), False);
      //IdDir := IntToStr(Count + 1) + '/';
      NewTimerClock := AddTimer;
      NewTimerClock.Caption :=
        string(Conf.GetValue(UTF8Decode(TIMER_CONF_TITLE), DEF_TIMER_TITLE));
      {Hours := Conf.GetValue(UTF8Decode(TIMER_CONF_HOURS), 0);
      Mins := Conf.GetValue(UTF8Decode(TIMER_CONF_MINUTES), 0);
      Secs := Conf.GetValue(UTF8Decode(TIMER_CONF_SECONDS), 0);

      NewTimerClock.Duration :=
        EncodeTime(Hours, Mins, Secs, 0);}

      // When float is saved, it is saved as
      fs := FormatSettings;
      fs.DecimalSeparator := '.';
      //ErrorText := (string(Conf.GetValue(UTF8Decode(TIMER_CONF_DURATION), '0')));
      NewTimerClock.Duration :=
        StrToFloat(string(Conf.GetValue(UTF8Decode(TIMER_CONF_DURATION), '0')), fs);
      NewTimerClock.IsProgressOnIcon :=
        Conf.GetValue(UTF8Decode(TIMER_CONF_NOTIFIER), False);

      {Success := NewTimerClock.SetAudioFile(
        string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOFILE), '')),
        StrToFloat(string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOLENGTH), '0'))),
        ErrorText);
      if not Success then
      begin
        NewTimerClock.SetAudioFile('', 0, ErrorText);
      end;
      newTimerClock.Audio.Looped := Conf.GetValue(TIMER_CONF_AUDIOLOOP, False);}

      if TAudio.Loaded then
      begin
        try
          NewTimerClock.Audio.FileName :=
            string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOFILE), ''));
          NewTimerClock.Audio.Looped := Conf.GetValue(TIMER_CONF_AUDIOLOOP, False);
        except
          on E: EInvalidAudio do
          begin
            ErrorText := ErrorText + 'Could not load audio file ' +
              string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOFILE), '')) +
              ' - unsupported format or invalide file. File name will be reset to blank.'#13#10;
          end
          else
            ErrorText := ErrorText + 'Could not load audio file ' +
              string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOFILE), '')) +
              ' - unknown error. File name will be reset to blank.'#13#10;
        end;
      end
      else
      begin
        NewTimerclock.AudioInfo.FileName :=
          string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOFILE), ''));
        NewTimerClock.AudioInfo.Duration :=
          StrToFloat(string(Conf.GetValue(UTF8Decode(TIMER_CONF_AUDIOLENGTH), '0')), fs);
        NewTimerClock.AudioInfo.Looped := Conf.GetValue(TIMER_CONF_AUDIOLOOP, False);
      end;
      NewTimerClock.ModalAlert :=
        Conf.GetValue(TIMER_CONF_MODALALERT, False);
      NewTimerClock.TrayNotification :=
        Conf.GetValue(TIMER_CONF_TRAYNOTIFiCATION, False);

      //NewTimerClock.AddSubscription(FFormWidget);
      //NewTimerClock.Widget.OnSelect:=@ClockSelected;
      PostTimerCreation(NewTimerClock);

      {Order := TIdList.Create;
      OrderString := TStringList.Create;

      if not Conf.GetValue(TIMER_CONF_ORDER, OrderString, '0') then
        ShowMessage('Getting order failed');

      for Pos in OrderString do
      begin
        Order.Add(StrToInt(Pos));
      end;  }
      Conf.CloseKey;
    end;

    Conf.Free;
    //OrderString.Free;
    //Order.Free;
    Reorder;

    if ErrorText = '' then
      StatusMessage := 'Saved timers loaded.'
    else
    begin
      StatusMessage := 'Saved timers loaded (with errors).';
      ShowMessage(ErrorText);
    end;
    UpdateStatusTimerCount;
  end;
end;

function TfrmMain.AddTimer(): TfraTimer;
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
  NewWidget.TitleEditable := GlobalUserConfig.AllowTimerTitleEdit;
  //NewWidget.OnTimerStart := @TimerStarted;
  //NewWidget.OnTimerPause := @TimerPaused;
  //NewWidget.OnTimerStop := @TimerFinished;
  NewWidget.imgTimer.Picture.Assign(FWidgetStoppedBitmap);
  NewWidget.LastProgressIconIndex := LAST_TRAY_ICON_DEFAULT;
  //NewWidget.LastProgressPercent:=0;

  //NewWidget.OnTimerProgressUpdate := @TimerProgressUpdated;
  {if not GlobalUserConfig.AllowTimerTitleEdit then
  begin
    NewWidget.edtTitle.Color:=clForm;
    NewWidget.edtTitle.ReadOnly:=True;
    //NewWidget.edtTitle.ParentColor:=True;
  end;}
  FTimerFrames.Add(Id, NewWidget);
  FOrder.Insert(0, Id);
  Reorder;

  //NewTimer.Widget := NewWidget;
  //NewWidget.OnNotifyChange := @NotifyChange;
  //NewWidget.OnProgressOnIconChanged := @HandleTimerFrameIconProgressChange;
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

{procedure TfrmMain.NotifyChange(Sender: TObject);
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
procedure TfrmMain.HandleTimerFrameIconProgressChange(Sender: TfraTimer);
var
  Temp: TfraTimer;
  Count: integer;
  //TODO: This variable is not needed now?
  //ForcefulUncheck: boolean;
begin
  //Sender := TfraTimer(Sender);
  { Tray checkboxes can be checked by only one timer at time, as the status
  of only one timer can be shown in the System Tray.
  Tray checkboxes can be changed by
  1 - Unchecking a checked timer. This would mean that no timers are now
      publishing the progrss. All are unchecked.
  2 - Checking another timer. This would mean that the checked timer now
      becomes active and the previous one gets unchecked.
  3 - No timers were checked before, checking one. There is nothing to be done
      in this case.
  }

  {This is option 1}
  if not Sender.IsProgressOnIcon then
  begin
    Sender.PublishProgress(TIMER_PROGRESS_OFFTRAY);
    //Notifier.IsProgressOnIcon := False;
    Exit;
  end;

  {This is to handle option 2.
  We also check if it a case of option 3 using variable ForcefulUncheck.
  Note that in this option,
    a) If the timer that is taking over is in running state, then the
       previously checked timer need to publish progress, as the new one
       will forcefully override and take over.
    b) If the timre that is taking over is not running, then the progress
       has to be published for the previous one.
  }
  //ForcefulUncheck := False;
  // For all timers other than the sender, uncheck if checked
  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    Temp := FTimerFrames.Data[Count];
    if Temp <> Sender then
      if Temp.IsProgressOnIcon then
      begin
        Temp.CallbackOnProgressOnIconChange := False;
        Temp.IsProgressOnIcon := False;
        Temp.CallbackOnProgressOnIconChange := True;
        //ForcefulUncheck := True;
        { If the timer that has been checked is not running, then
        we have a special case where there is no one to take over and
        override the publishing. So manually take the previously running one
        off the tray.}
        if not Sender.Running then
          Temp.PublishProgress(TIMER_PROGRESS_OFFTRAY);
      end;
  end;


end;

procedure TfrmMain.SaveClocks(Conf: TJsonConfig);
var
  TimerClock: TfraTimer;
  Count: integer;
  OrderStrings: TStringList;
  //Order: TIdList;
  Id: longword;
  fs: TFormatSettings;
  //Index: integer;
begin
  //Order := TIdList.Create;
  OrderStrings := TStringList.Create;

  //GetOrder(Order);
  {for Id in FOrder do
  begin
    Order.Add(Id);
  end;}

  // While saving, existing IDs of clocks are ignored.
  // New IDs are generated in sequence.

  Conf.SetValue(TIMER_CONF_COUNT, FTimerFrames.Count);

  if FOrder.Count <> FTimerFrames.Count then
    ShowMessage('FOrder.Count does not match FTimerFrames.Count');


  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    Conf.OpenKey(UTF8Decode(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) + '/'), True);
    // FOrder has the order of IDs, but in reverse order.
    Id := FOrder[FTimerFrames.Count - Count - 1];
    TimerClock := FTimerFrames.KeyData[Id];

    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    Conf.SetValue(UTF8Decode(TIMER_CONF_TITLE), UTF8Decode(TimerClock.Caption));
    {Conf.SetValue(TIMER_CONF_HOURS, HourOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_MINUTES,
      MinuteOf(TimerClock.Duration));
    Conf.SetValue(TIMER_CONF_SECONDS,
      SecondOf(TimerClock.Duration));}
    //Conf.SetValue(TIMER_CONF_DURATION, TimerClock.Duration);
    //Conf.SetValue(TIMER_CONF_NOTIFIER,
    //  TimerClock.Audio.Looped);

    fs := FormatSettings;
    fs.DecimalSeparator := '.';
    Conf.SetValue(UTF8Decode(TIMER_CONF_DURATION), UTF8Decode(FloatToStr(TimerClock.Duration, fs)));
    Conf.SetValue(UTF8Decode(TIMER_CONF_NOTIFIER), TimerClock.TrayNotification);
    if TAudio.Loaded then
    begin
      Conf.SetValue(UTF8Decode(TIMER_CONF_AUDIOFILE),
        UTF8Decode(TimerClock.Audio.FileName));
      Conf.SetValue(UTF8Decode(TIMER_CONF_AUDIOLENGTH),
        UTF8Decode(FloatToStr(TimerClock.Audio.Duration, fs)));
      Conf.SetValue(TIMER_CONF_AUDIOLOOP,
        TimerClock.Audio.Looped);
    end
    else
    begin
      Conf.SetValue(UTF8Decode(TIMER_CONF_AUDIOFILE),
        UTF8Decode(TimerClock.AudioInfo.FileName));
      Conf.SetValue(UTF8Decode(TIMER_CONF_AUDIOLENGTH),
        UTF8Decode(FloatToStr(TimerClock.AudioInfo.Duration, fs)));
      Conf.SetValue(TIMER_CONF_AUDIOLOOP,
        TimerClock.AudioInfo.Looped);
    end;


    Conf.SetValue(TIMER_CONF_MODALALERT,
      TimerClock.ModalAlert);
    Conf.SetValue(TIMER_CONF_TRAYNOTIFiCATION,
      TimerClock.TrayNotification);

    // Add the new ID at the beginning of the list
    OrderStrings.Insert(0, IntToStr(Count + 1));

    //Index := FOrder.IndexOf(TimerClock.Id);
    //Assert(Index >= 0);
    //Order.Items[Index] := Count + 1;
    Conf.CloseKey;
  end;


  {for Id in FOrder do
    OrderStrings.Add(IntToStr(Id));}

  Conf.SetValue(TIMER_CONF_ORDER, OrderStrings);
  //ShowMessage('After Savetofile ' + IntToStr(FOrder.Count));
  OrderStrings.Free;
  //Order.Free;
end;

procedure TfrmMain.DeleteSelected;
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
      TimerClock.Stop(False);

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

procedure TfrmMain.RemoveTimer(IdNew: longword);
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

procedure TfrmMain.MoveSelectedClocksUp;
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

procedure TfrmMain.MoveSelectedClocksDown;
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

procedure TfrmMain.OnShortTimer(Sender: TObject);
var
  TimerFrame: TfraTimer;
begin
  //EnterCriticalSection(TimerCriticalSection);
  //for TimerFrame in FActiveTimerFrames.K;
  //DebugLn('Timer fired. ');
  //DebugLn('' + IntToStr(FActiveTimerFrames.Count) + ' active timers');

  try
    if FActiveTimerFrames.Count = 0 then
      Exit;
    try
      for TimerFrame in FActiveTimerFrames do
      begin
        TimerFrame.HandleTimerTrigger();
      end;
    except
      on E: Exception do
        ShowMessage('Error (4): ' + E.ClassName + #13#10 + E.Message);
    end;
  finally
    //LeaveCriticalSection(TimerCriticalSection);
  end;

end;

procedure TfrmMain.AfterShow(var Msg: TLMessage);
begin
  if FTimerFrames.Count = 0 then
    aiNewTimer.Execute;
end;

procedure TfrmMain.AfterShow2(Data: PtrInt);
begin
  if FTimerFrames.Count = 0 then
    aiNewTimer.Execute;
end;

procedure TfrmMain.ShowModalAlert(Data: PtrInt);
begin
  if frmAlert.WindowState = wsMinimized then
    frmAlert.WindowState := wsNormal;
  { If the modal window is already showing, an exception is thrown when you
  attempt to ShowModal. So in that case, ShowOnTop }
  if not frmAlert.Showing then
    frmAlert.ShowModal
  else
    frmAlert.ShowOnTop;
end;

end.
