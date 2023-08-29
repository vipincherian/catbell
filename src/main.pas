{

Copyright (C) 2021 Vipin Cherian

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

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ActnList, ExtCtrls, Buttons, LCLIntf, LCLType,
  settings, optionsform, aboutform, BGRABitmap,
  BGRABitmapTypes, FPimage, timeralertform, dateutils, jsonConf,
  timerframe, fgl, util, editform, Math, StdCtrls, constants,
  {$IF defined(windows) }
  ShlObj, comobj, Win32Int, InterfaceBase,
  {$ENDIF}
  audio, metronome, log, FileInfo;

type
  //TTimerFrameMap = specialize TFPGMap<longword, TfraTimer>;
  //TIdList = specialize TFPGList<longword>;
  //TTimerFrameList = specialize TFPGList<TfraTimer>;

  { TfrmMain }

  TfrmMain = class(TForm)
    aiHelp: TAction;
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
    imgVolumeOn: TImage;
    imgVolumeOff: TImage;
    lblVolume: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem9: TMenuItem;
    pnlClocks: TPanel;
    pmiQuit: TMenuItem;
    pmiShowWindow: TMenuItem;
    pmiExit: TMenuItem;
    MenuItem8: TMenuItem;
    miToolsOptions: TMenuItem;
    miTools: TMenuItem;
    MenuItem3: TMenuItem;
    miHelpContents: TMenuItem;
    miAbout: TMenuItem;
    mmMain: TMainMenu;
    miAlarm: TMenuItem;
    miNewTimer: TMenuItem;
    pnlBorder: TPanel;
    pmTray: TPopupMenu;
    pmMain: TPopupMenu;
    sbxClocks: TScrollBox;
    sdgExport: TSaveDialog;
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
    tbVolume: TTrackBar;
    procedure aiHelpExecute(Sender: TObject);
    procedure aiDeleteTimerExecute(Sender: TObject);
    procedure aiExportExecute(Sender: TObject);
    procedure aiMoveDownExecute(Sender: TObject);
    procedure aiMoveUpExecute(Sender: TObject);
    procedure aiAboutExecute(Sender: TObject);
    procedure aiNewTimerExecute(Sender: TObject);
    procedure aiOptionsExecute(Sender: TObject);
    procedure aiQuitExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pmiShowWindowClick(Sender: TObject);
    procedure pnlClocksMouseUp(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure pnlClocksResize(Sender: TObject);
    procedure sbxClocksMouseUp(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure tbProgressAutoClick(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);

    procedure tiMainClick(Sender: TObject);
    procedure tiMainDblClick(Sender: TObject);
    procedure tiMainMouseUp(Sender: TObject; Button: TMouseButton;
    {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: integer);
    procedure uqiMainOtherInstance(Sender: TObject;
    {%H-}ParamCount: integer; const {%H-}Parameters: array of string);

  private
    { private declarations }
    FTrayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FAppProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    FWidgetProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    {$IF defined(windows) }
    FOverlayProgressIcons: array[1..TRAY_PROGRESS_ICON_COUNT] of TIcon;
    {$ENDIF}

    FTrayStoppedBitmap, FAppStoppedBitmap, FWidgetStoppedBitmap: TIcon;
    FLastTrayIconIndex: integer;
    FLastTrayPercent: integer;

    {$IF defined(windows) }
    AppHandle: THandle;
    FTaskBarList: ITaskbarList3;
    {$ENDIF}

    FDbFileName: string;
    FDbDefault: boolean;

    //FTimerFrames: TTimerFrameMap;
    FTimerFrames: TTimerFrameList;
    FActiveTimerFrames: TTimerFrameList;
    //FOrder: TIdList;
    FCounterClockID: TSequence;

    FShortTimer: TTimer;

    { This is an invisible timer frame, the purpose of which
    is to aid in resizing header sections when there are no timers
    added. It is a waste of space, and altogether inelegant,
    but I could not find a better and reliable way to achieve
    the same functionality.}
    FReference: TfraTimer;

    FReportStale: boolean;

    procedure CreateBitmaps;
    function GetStatusMessage: string;
    procedure LayoutControls;
    procedure PostTimerCreation({%H-}AValue: TfraTimer);
    procedure SetListButtonsStatus;
    procedure ResizeHeaderSections;

    function GetAnySelected: boolean;
    function QueryCanSelectedMoveDown: boolean;
    function QueryCanSelectedMoveUp: boolean;
    //procedure Reorder;
    procedure SetStatusMessage(AValue: string);
    procedure ShowInForeground;
    procedure UpdateStatusTimerCount;

  public
    procedure ClockSelected(Sender: TfraTimer);
    procedure TimerFinished(Sender: TfraTimer; UserInitiated: boolean);
    procedure TimerPaused(Sender: TfraTimer);
    procedure TimerStarted(Sender: TfraTimer);
    procedure ProgressUpdate(Widget: TfraTimer; Progress: single);
    procedure OptionsFormClosed();
    function GetExportFileName: string;

    procedure ProcessCommandline;
    procedure SavetoFile;
    procedure LoadfromFile;

    function AddTimer(): TfraTimer;
    procedure HandleTimerFrameIconProgressChange(Sender: TfraTimer);
    procedure SaveClocks(Conf: TJsonConfig);
    procedure DeleteSelected;
    property AnySelected: boolean read GetAnySelected;
    //procedure RemoveTimer(IdNew: longword);
    procedure RemoveTimer(Widget: TfraTimer);
    procedure MoveSelectedClocksUp;
    procedure MoveSelectedClocksDown;
    procedure OnShortTimer(Sender: TObject);
    procedure AfterShow({%H-}Data: PtrInt);
    procedure ShowModalAlert({%H-}Data: PtrInt);
    procedure AddAlert({%H-}Data: PtrInt);
    property StatusMessage: string read GetStatusMessage write SetStatusMessage;

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Info: TFileVersionInfo;
begin
  { Obtain the application handle, and the taskbar COM object.
  This has to be done at the beginning, as calls are made to progressupdate
  from within FormCreate. This cannot be pushed down }

  {$IF defined(windows) }
  AppHandle := TWin32WidgetSet(WidgetSet).{%H-}AppHandle; // Omit the warning

  FTaskBarList := CreateComObject(CLSID_TaskbarList) as ITaskbarList3;
  {$ENDIF}

  //{ TODO : Merge these calls - SoundPool should load default audio too. }
  //AudioSystem.LoadDefaultSounds;
  SoundPool.LoadAllDefaultSounds;

  //FOrder := TIdList.Create;
  Constraints.MinWidth := FORM_MIN_WIDTH;
  Constraints.MinHeight := FORM_MIN_HEIGHT;
  ;

  //FTimerFrames := TTimerFrameMap.Create;
  FTimerFrames := TTimerFrameList.Create;
  FActiveTimerFrames := TTimerFrameList.Create;

  FCounterClockID := TSequence.Create;

  FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
  FLastTrayPercent := 0;

  FDbDefault := True;

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

  tiMain.Icon.Assign(FTrayStoppedBitmap);
  tiMain.Visible := UserConfig.ShowTrayIcon;

  stbMain.BeginUpdate;
  if AudioSystem.Loaded then
    stbMain.Panels[PANEL_AUDIO].Text := 'Audio: Ok'
  else
    stbMain.Panels[PANEL_AUDIO].Text := 'Audio: Error';
  stbMain.EndUpdate;

  FReportStale := False;

  if AudioSystem.Loaded then
  begin
    AudioSystem.Volume := UserConfig.Volume;
    tbVolume.Position := AudioSystem.Volume;
  end
  else
  begin
    { There is no need to set the position of tbVolume to 0. If set, this will
    trigger the callback, which would in turn set the volume of the audio
    system.}
    tbVolume.Enabled := False;
  end;

  imgVolumeOff.Left := imgVolumeOn.Left;
  imgVolumeOff.Top := imgVolumeOn.Top;

  Logger.Subscribe(@SetStatusMessage);

  Info := TFileVersionInfo.Create(Self);
  Info.ReadFileInfo;
  Caption := Info.VersionStrings.Values['ProductName'] + ' ' +
    Info.VersionStrings.Values['FileVersion'];
  Info.Free;

  pnlClocks.Caption := '';

  UserInterfaceMetrics.ReferenceForm := Self;
  LayoutControls;

end;

procedure TfrmMain.aiNewTimerExecute(Sender: TObject);
var
  Added: TfraTimer;
begin
  with UserConfig do
  begin
    frmEdit.Duration := DefaultTimerDuration;

    frmEdit.Description := DefaultTimerTitle;
    frmEdit.TrayNotification := ShowTrayAlert;
    frmEdit.ModalAlert := ShowModalAlert;
    { We are not providing an option to keep audio looped by default }
    frmEdit.ckbLoop.Checked := False;
  end;

  if not AudioSystem.Loaded then
    frmEdit.SoundLooped := False;

  if frmEdit.ShowForAdd then
  begin
    Added := AddTimer;
    Added.Caption := frmEdit.Description;
    Added.dtpSet.Time := frmEdit.Duration;
    Added.ModalAlert := frmEdit.ModalAlert;
    Added.TrayNotification := frmEdit.TrayNotification;

    if AudioSystem.Loaded then
    begin
      Added.SoundLooped := frmEdit.ckbLoop.Checked;
      Added.Metronome := frmEdit.Metronome;
    end;
    PostTimerCreation(Added);
    SavetoFile;
  end;
end;

procedure TfrmMain.aiOptionsExecute(Sender: TObject);
{$IF defined(windows) }
var
  OldTaskbarIconType: TTaskbarIconType;
{$ENDIF}
begin
  { Take a backup of options that need to be watched for change }
  {$IF defined(windows) }
  OldTaskbarIconType := UserConfig.TaskbarIconType;
  {$ENDIF}
  frmOptions.ShowModal;
  OptionsFormClosed;

  { if taskbar icon type has been changed, then artificially trigger the
  timer event }
  {$IF defined(windows) }
  if OldTaskbarIconType <> UserConfig.TaskbarIconType then
  begin
    if UserConfig.TaskbarIconType = TaskbarAppIcon then
      FTaskBarList.SetOverlayIcon(AppHandle, 0, pwidechar(''));
    FReportStale := True;
    if FShortTimer.Enabled then
      OnShortTimer(Self);
  end;
  {$ENDIF}
end;

procedure TfrmMain.aiQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  CurrPosNormal, CurrPosRestored: TRect;
  //Count: integer;
  //StartTickCount: QWord;
  Timer: TfraTimer;
begin
  { if any audio is playing, stop }
  if AudioSystem.Loaded then
  begin
    StatusMessage := 'Stopping sounds being played if any...';
    Cursor := crHourglass;
    //for Count := 0 to FTimerFrames.Count - 1 do
    for Timer in FTimerFrames do
    begin

      Timer.AbortSound(True);

      //StartTickCount := GetTickCount64;
      //{ Abort is asynchronous, wait till each timer aborts.
      //Also, we do not wait for more than two seconds per timer.
      //After that, it is past caring. Tardiness can be tolerated only as much. }
      //while Timer.IsSoundPlaying do
      //begin
      //  Logger.Debug('Waiting for frame to stop audio');
      //  Application.ProcessMessages;
      //  if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
      //    break;
      //end;
    end;
  end;

  CurrPosNormal.Top := Top;
  CurrPosNormal.Left := Left;
  CurrPosNormal.Width := Width;
  CurrPosNormal.Height := Height;

  CurrPosRestored.Top := RestoredTop;
  CurrPosRestored.Left := RestoredLeft;
  CurrPosRestored.Width := RestoredWidth;
  CurrPosRestored.Height := RestoredHeight;

  with UserConfig do
  begin
    LastPosNormal := CurrPosNormal;
    LastPosRestored := CurrPosRestored;

    AutoProgress := tbProgressAuto.Down;
    LastWindowState := WindowState;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if UserConfig.QueryExit then
    CanClose := MessageDlg('Do you really want to close the application?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes;

end;

procedure TfrmMain.aiAboutExecute(Sender: TObject);
begin
  frmAbout.ShowModal;
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
  SavetoFile;
end;

procedure TfrmMain.aiMoveUpExecute(Sender: TObject);
begin
  MoveSelectedClocksUp;
  SetListButtonsStatus;
  SavetoFile;
end;

procedure TfrmMain.aiDeleteTimerExecute(Sender: TObject);
begin
  DeleteSelected;
  SetListButtonsStatus;
  SavetoFile;
end;

procedure TfrmMain.aiHelpExecute(Sender: TObject);
begin
  OpenUrl(HELP_URL);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Count: integer;
  //I: integer;
  Timer: TfraTimer = nil;
begin
  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin
    FTrayProgressIcons[Count].Free;
    FAppProgressIcons[Count].Free;
    FWidgetProgressIcons[Count].Free;
    {$IF defined(windows) }
    FOverlayProgressIcons[Count].Free;
    {$ENDIF}
  end;
  FTrayStoppedBitmap.Free;
  FAppStoppedBitmap.Free;
  FWidgetStoppedBitmap.Free;


  SavetoFile;

  //for I := 0 to FTimerFrames.Count - 1 do
  //begin
  //  FTimerFrames.Data[i].Free;
  //end;

  while FTimerFrames.Count > 0 do
  begin
    Timer := FTimerFrames.First;
    FTimerFrames.Remove(Timer);
    Timer.Free;
  end;
  FCounterClockID.Free;

  FReference.Free;

  FActiveTimerFrames.Free;
  FTimerFrames.Free;

  //FOrder.Free;

  FShortTimer.Free;

end;

procedure TfrmMain.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin
    pmMain.PopUp;
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  ;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  if not AudioSystem.Loaded then
  begin
    MessageDlg('Audio not loaded.' + LineEnding +
      'Alarms and other dependent functionalities will not work.' +
      LineEnding + LineEnding + AudioSystem.Error,
      mtWarning, [mbOK], 0);
    tbVolume.Position := 0;
  end
  else if AudioSystem.Volume <= 0 then
  begin
    if QuestionDlg('Audio Muted',
      'Audio seems to be working, but volume is set to zero.' +
      ' Reset volume to default levels?', mtConfirmation,
      [mrYes, 'Reset volume', mrNo, 'Keep muted'], '') = mrYes then
      { This in turn sets the volume of the audio system }
      tbVolume.Position := DEFAULT_VOLUME;
  end;

  with UserConfig do
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

    Application.QueueAsyncCall(@AfterShow, 0);

    tbProgressAuto.Down := AutoProgress;

  end;
end;

procedure TfrmMain.pmiShowWindowClick(Sender: TObject);
begin
  ShowInForeground;
end;

procedure TfrmMain.pnlClocksMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin
    pmMain.PopUp;
  end;
end;

procedure TfrmMain.pnlClocksResize(Sender: TObject);
begin
  ResizeHeaderSections;
end;

procedure TfrmMain.sbxClocksMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbRight then
  begin
    pmMain.PopUp;
  end;
end;


procedure TfrmMain.tbProgressAutoClick(Sender: TObject);
begin
  UserConfig.AutoProgress := tbProgressAuto.Down;
end;

procedure TfrmMain.tbVolumeChange(Sender: TObject);
begin
  lblVolume.Caption := IntToStr(tbVolume.Position) + '%';
  AudioSystem.Volume := tbVolume.Position;
  //imgVolumeOn.Visible := (tbVolume.Position > 0);
  //imgVolumeOff.Visible := (not imgVolumeOn.Visible);
  { Change the anchor sibling of volume slider so that it does not grow }
  if tbVolume.Position > 0 then
  begin
    tbVolume.AnchorSide[akTop].Control := imgVolumeOn;
    imgVolumeOn.Visible := True;
    imgVolumeOff.Visible := False;
  end
  else
  begin
    tbVolume.AnchorSide[akTop].Control := imgVolumeOff;
    imgVolumeOn.Visible := False;
    imgVolumeOff.Visible := True;
  end;

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

procedure TfrmMain.uqiMainOtherInstance(Sender: TObject; ParamCount: integer;
  const Parameters: array of string);
begin
  { If an attempt was made to start another instance of this application,
  bring the main form to the front }
  Application.BringToFront;
  Logger.Info('Attempt to start another instance. Brought window to front.');
end;


procedure TfrmMain.PostTimerCreation(AValue: TfraTimer);
begin
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

  TrayIconSize := UserConfig.TrayIconSize;
  AppIconSize := UserConfig.AppIconSize;

  { Read the image in resources to a stream }
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

  HiresBmp.Free;
  Stream.Free;


  Stream := TResourceStream.Create(hInstance, '256_PROGRESS_BASE', RT_RCDATA);
  InSet := 48;

  for Count := 1 to TRAY_PROGRESS_ICON_COUNT do
  begin
    Stream.Seek(0, soFromBeginning);
    HiResBmp := TBGRABitmap.Create(Stream);

    with HiResBmp do
    begin

      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;

      CanvasBGRA.Pie(Inset, Inset, LARGE_ICON_SIZE - Inset, LARGE_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER, -(15 * RAD_MULTIPLIER * (Count - 1)));

      { To give it a glossy feel, we try to add a translucent
      white sheen to the left semi-circle }
      CanvasBGRA.Brush.Opacity := PROGRESS_GLOSS_OPACITY;
      CanvasBGRA.Pen.Opacity := PROGRESS_GLOSS_OPACITY;
      CanvasBGRA.Brush.Color := clWhite;
      CanvasBGRA.Pen.Color := clWhite;
      CanvasBGRA.Pie(Inset, Inset, LARGE_ICON_SIZE - Inset, LARGE_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER,
        { We need to draw only half the circle, or the current pie,
        whichever is lesser. } -(15 * RAD_MULTIPLIER * Max(
        (Count - 1), TRAY_PROGRESS_ICON_COUNT div 2))
        );
    end;

    FinalBmp := HiresBmp.Resample(TrayIconSize, TrayIconSize, rmFineResample) as
      TBGRABitmap;

    FTrayProgressIcons[Count] := TIcon.Create;
    FTrayProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    FinalBmp := HiresBmp.Resample(AppIconSize, AppIconSize, rmFineResample) as
      TBGRABitmap;

    FAppProgressIcons[Count] := TIcon.Create;
    FAppProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    FinalBmp := HiresBmp.Resample(WIDGET_ICON_WIDTH, WIDGET_ICON_WIDTH,
      rmFineResample) as TBGRABitmap;

    FWidgetProgressIcons[Count] := TIcon.Create;
    FWidgetProgressIcons[Count].Assign(FinalBmp.Bitmap);

    FinalBmp.Free;

    HiresBmp.Free;


    {$IF defined(windows) }
    { Create overlay icons, only in the case of windows }
    HiResBmp := TBGRABitmap.Create(LARGE_ICON_SIZE, LARGE_ICON_SIZE);
    with HiResBmp do
    begin
      CanvasBGRA.Brush.Color := OVERLAY_BACKGROUND_COLOUR;
      CanvasBGRA.Pen.Color := OVERLAY_STROKE_COLOUR;
      CanvasBGRA.Pen.Width := OVERLAY_STROKE_WIDTH;

      CanvasBGRA.Ellipse(OVERLAY_STROKE_WIDTH div 2,
        OVERLAY_STROKE_WIDTH div 2,
        LARGE_ICON_SIZE - (OVERLAY_STROKE_WIDTH div 2),
        LARGE_ICON_SIZE - (OVERLAY_STROKE_WIDTH div 2));


      CanvasBGRA.Brush.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pen.Color := PROGRESS_COLOUR;
      CanvasBGRA.Pie(Inset, Inset, LARGE_ICON_SIZE - Inset, LARGE_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER, -(15 * RAD_MULTIPLIER * (Count - 1)));

      { To give it a glossy feel, we try to add a translucent
      white sheen to the left semi-circle }
      CanvasBGRA.Brush.Opacity := PROGRESS_GLOSS_OPACITY;
      CanvasBGRA.Pen.Opacity := PROGRESS_GLOSS_OPACITY;
      CanvasBGRA.Brush.Color := clWhite;
      CanvasBGRA.Pen.Color := clWhite;
      CanvasBGRA.Pie(Inset, Inset, LARGE_ICON_SIZE - Inset, LARGE_ICON_SIZE - Inset,
        90 * RAD_MULTIPLIER,
        { We need to draw only half the circle, or the current pie,
        whichever is lesser. } -(15 * RAD_MULTIPLIER * Max(
        (Count - 1), TRAY_PROGRESS_ICON_COUNT div 2))
        );
    end;

    FinalBmp := HiresBmp.Resample(TrayIconSize, TrayIconSize, rmFineResample) as
      TBGRABitmap;

    FOverlayProgressIcons[Count] := TIcon.Create;
    FOverlayProgressIcons[Count].Assign(FinalBmp.Bitmap);
    FinalBmp.Free;

    HiresBmp.Free;
    {$ENDIF}
  end;
  Stream.Free;
end;

function TfrmMain.GetStatusMessage: string;
begin
  Result := stbMain.Panels[PANEL_MESSAGE].Text;
end;

procedure TfrmMain.LayoutControls;
begin
  {Dynamically fix pacing}
  bbMoveDown.BorderSpacing.Bottom := UserInterfaceMetrics.Padding;
  bbMoveDown.BorderSpacing.Right := UserInterfaceMetrics.Margin;

  lblVolume.BorderSpacing.Bottom := UserInterfaceMetrics.Margin;
  tbVolume.BorderSpacing.Bottom := UserInterfaceMetrics.Padding;

  imgVolumeOn.BorderSpacing.Top := UserInterfaceMetrics.Margin;
  imgVolumeOff.BorderSpacing.Top := UserInterfaceMetrics.Margin;


  pnlBorder.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  pnlBorder.BorderSpacing.Left := UserInterfaceMetrics.Margin;
  pnlBorder.BorderSpacing.Top := UserInterfaceMetrics.Padding;
  //Self.BorderSpacing.InnerBorder := 10;
  //BorderSpacing.Around:=10;
  //bbMoveDown.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  //bbMoveDown.BorderSpacing.Bottom := UserInterfaceMetrics.Padding;
  //bbDelete.BorderSpacing.Bottom := UserInterfaceMetrics.Padding;
end;

procedure TfrmMain.SetListButtonsStatus;
begin
  bbDelete.Enabled := AnySelected;
  tbDelete.Enabled := AnySelected;
  bbMoveUp.Enabled := QueryCanSelectedMoveUp;
  bbMoveDown.Enabled := QueryCanSelectedMoveDown;
  tbMoveUp.Enabled := QueryCanSelectedMoveUp;
  tbMoveDown.Enabled := QueryCanSelectedMoveDown;
end;

procedure TfrmMain.ResizeHeaderSections;
var
  //Id: longword;
  Timer: TfraTimer;
  Filled: integer = 0;
  Temp: integer;
begin
  //if FOrder.Count > 0 then
  //begin
  //  Id := FOrder.Items[0];
  //  Timer := FTimerFrames.KeyData[Id];
  //end
  //else
  //begin
  //  Timer := FReference;
  //end;

  if FTimerFrames.Count > 0 then
    Timer := FTimerFrames.First
  else
    Timer := FReference;

  hdrTimers.Sections.Items[0].Width :=
    Timer.cbSelect.Left + Timer.cbSelect.Width + (UserInterfaceMetrics.Padding div 2);
  Inc(Filled, hdrTimers.Sections.Items[0].Width);

  Temp := Timer.dtpSet.Left + Timer.dtpSet.Width;
  Inc(Temp, ((Timer.bbPlay.Left - Temp) div 2));

  { If there are no timers added, we are using FReference as a reference.
  It is an invisible frame and is not under the scrollbar hence its width and
  the scrollbar's may not be the same. This needs to be taken into account,
  and an adjustment of (sbxClocks.Width - Timer.Width) needs to be made.}

  Inc(Temp, sbxClocks.ClientWidth - Timer.Width - Filled);

  hdrTimers.Sections.Items[1].Width := Temp;

  Inc(Filled, hdrTimers.Sections.Items[1].Width - (sbxClocks.ClientWidth - Timer.Width));

  Temp := Timer.bbAdjust.Left + Timer.bbAdjust.Width;
  Temp := Temp + ((Timer.lblCountdown.Left - Temp) div 2) - Filled;
  hdrTimers.Sections.Items[2].Width := Temp;
  Inc(Filled, Temp);

  Temp := Timer.lblCountdown.Left + Timer.lblCountdown.Width;
  Temp := Temp - Filled + (UserInterfaceMetrics.Padding div 2);
  hdrTimers.Sections.Items[3].Width := Temp;
  Inc(Filled, Temp);

  Temp := Timer.ckbIconProgress.Left + Timer.ckbIconProgress.Width;
  Temp := Temp - Filled + (UserInterfaceMetrics.Padding * 4) -
    (UserInterfaceMetrics.Padding div 2);
  hdrTimers.Sections.Items[4].Width := Temp;
  Inc(Filled, Temp);

  Temp := Timer.Width - Filled;
  hdrTimers.Sections.Items[5].Width := Temp;
  Inc(Filled, Temp);
end;

function TfrmMain.GetAnySelected: boolean;
var
  //Count: integer;
  Clock: TfraTimer;
begin
  //for Count := 0 to FTimerFrames.Count - 1 do
  Result := False;
  for Clock in FTimerFrames do
  begin
    //Clock := FTimerFrames.Data[Count];
    //if Clock = nil then
    //  ShowMessage('Clock is Nil');
    if Clock.Selected then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TfrmMain.QueryCanSelectedMoveDown: boolean;
var
  //Id: longword;
  Clock: TfraTimer;
  EncounteredSelected: boolean;
begin

  EncounteredSelected := False;

  { Code is more complicated than it seems. BEGIN END blocks are necessary }
  for Clock in FTimerFrames do
  begin
    if not Clock.Selected then
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

function TfrmMain.QueryCanSelectedMoveUp: boolean;
var
  //Id: longword;
  EncounteredUnselected: boolean;
  Clock: TfraTimer;
begin
  EncounteredUnselected := False;

  { Code is more complicated than it seems. BEGIN END blocks are necessary }
  for Clock in FTimerFrames do
  begin
    if Clock.Selected then
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


//procedure TfrmMain.Reorder;
//var
//  Id: longword;
//  TimerWidget, PreviousWidget: TfraTimer;
//  Filled: integer;
//  CountTabOrder: integer;
//  Count: integer;
//begin
  {Filled := hdrTimers.Height;
  CountTabOrder := 0;
  hdrTimers.Top := 0;
  //for Id in FOrder do

  TimerWidget := FTimerFrames.First;

  TimerWidget.AnchorSide[akTop].Side := asrTop;
  TimerWidget.AnchorSide[akTop].Control := pnlClocks;

  TimerWidget.TabOrder := 0;

  for Count := 1 to FTimerFrames.Count - 1 do
  begin
    //TimerWidget := FTimerFrames.KeyData[Id];

    TimerWidget := FTimerFrames.Items[Count];
    PreviousWidget := FTimerFrames.Items[Count - 1];

    TimerWidget.AnchorSide[akTop].Side := asrBottom;
    TimerWidget.AnchorSide[akTop].Control := PreviousWidget;

    //TimerWidget.Top := Filled;
    TimerWidget.TabOrder := Count;//CountTabOrder;

    //Inc(Filled, TimerWidget.Height);
    //Inc(CountTabOrder);
  end;
  sbxClocks.ReAlign;
  pnlClocks.ReAlign;
  pnlClocks.Refresh; }

//end;

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
  stbMain.BeginUpdate;
  stbMain.Panels[PANEL_TIMERCOUNT].Text := TextMessage;
  stbMain.EndUpdate;
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

  Duration: TDateTime;
  DurationText: string;
begin

  Duration := Sender.Duration;

  Hours := HourOf(Duration);
  Minutes := MinuteOf(Duration);
  Seconds := SecondOf(Duration);

  try
    if FActiveTimerFrames.Remove(Sender) = -1 then
      Logger.Debug('Unable to remove from active timerframes.');

    FShortTimer.Enabled := (FActiveTimerFrames.Count > 0);

  except
    on E: Exception do
      ShowMessage('Error(1): ' + E.ClassName + LineEnding + E.Message);
  end;
  DurationText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  if Sender.TrayNotification and (not UserInitiated) and tiMain.Visible then
  begin
    tiMain.BalloonHint :=
      Sender.Caption + ' completed. (' + DurationText + ')';
    tiMain.ShowBalloonHint;
  end;

  if Sender.ModalAlert and (not UserInitiated) then
  begin
    //frmAlert.AddTimer(Sender);
    Application.QueueAsyncCall(@AddAlert, PtrInt(Sender));
    //Application.QueueAsyncCall(@ShowModalAlert, 0);
    //ShowModalAlert(0);
  end;
  UpdateStatusTimerCount;
end;

procedure TfrmMain.TimerPaused(Sender: TfraTimer);
begin
  try
    FActiveTimerFrames.Remove(Sender);
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
      Logger.Debug('Exception in TfrmMain.TimerPaused: ' + E.ClassName +
        LineEnding + E.Message);
  end;

end;

procedure TfrmMain.TimerStarted(Sender: TfraTimer);
var
  Index: integer;
begin
  try
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
      ShowMessage('Exception in TfrmMain.TimerStarted ' + E.ClassName +
        LineEnding + E.Message);
  end;

end;

procedure TfrmMain.ProgressUpdate(Widget: TfraTimer; Progress: single);
var
  Index: integer;
  {$IF defined(windows)}
  TaskbarPercent: integer;
  Result: HRESULT;
  {$ENDIF}
begin
  if Progress < 0 then
    Progress := 0;
  if (Progress > (TIMER_PROGRESS_FINISHED - 0.01)) and
    (Progress < (TIMER_PROGRESS_OFFTRAY + 0.01)) then
  begin
    {If the timer has stopped or has been taken off the tray then
    fix the tray and application icons accordingly.}
    tiMain.Icon.Assign(FTrayStoppedBitmap);
    Icon.Assign(FTrayStoppedBitmap);

    {$IF not defined(windows)}
    Assert(UserConfig.TaskbarIconType <> TaskbarOverlayIcon);
    {$ENDIF}

    if UserConfig.TaskbarIconType = TaskbarAppIcon then
      Application.Icon.Assign(FAppStoppedBitmap);

    {$IF defined(windows) }
    if UserConfig.TaskbarIconType = TaskbarOverlayIcon then
      FTaskbarList.SetOverlayIcon(AppHandle, 0, pwidechar(''));
    FTaskBarList.SetProgressState(AppHandle, TBPF_NOPROGRESS);
    {$ENDIF}

    FLastTrayIconIndex := LAST_TRAY_ICON_DEFAULT;
    FLastTrayPercent := 0;

    { Change the timer image only if it has really finished.}
    if (Widget <> nil) and (Progress < (TIMER_PROGRESS_FINISHED + 0.01)) then
    begin
      Widget.imgTimer.Picture.Assign(FWidgetStoppedBitmap);
      Widget.LastProgressIconIndex := LAST_TRAY_ICON_DEFAULT;
    end;
  end
  else
  begin
    Index := Floor(Progress * 24.0);
    {$IF defined(windows) }
    TaskbarPercent := Ceil(Progress * 100);
    {$ENDIF}

    if Index >= 24 then
      Index := 23;
    Assert((Index >= 0) and (Index < TRAY_PROGRESS_ICON_COUNT));
    if Widget = nil then
      Exit;
    if Widget.IsProgressOnIcon and tiMain.Visible then
    begin
      {Redraw icons only if there is a change}
      if (FLastTrayIconIndex <> Index) or FReportStale then
      begin
        tiMain.Icon.Assign(FTrayProgressIcons[Index + 1]);

        Icon.Assign(FTrayProgressIcons[Index + 1]);



        if UserConfig.TaskbarIconType = TaskbarAppIcon then
          Application.Icon.Assign(FAppProgressIcons[Index + 1]);
        {$IF defined(windows)}
        if UserConfig.TaskbarIconType = TaskbarOverlayIcon then
        begin

          Result := FTaskBarList.SetOverlayIcon(AppHandle,
            FOverlayProgressIcons[Index + 1].Handle, pwidechar(''));
          if Result <> S_OK then
            Logger.Debug('SetOverlayIcon failed ' + IntToStr(Result));

        end;
        {$ENDIF}


        FLastTrayIconIndex := Index;
      end;

      {In Windows, set the progress in task bar}
      {$IF defined(windows) }
      if (FLastTrayPercent <> TaskbarPercent) or FReportStale then
      begin
        if Widget.Running and (not Widget.Paused) then
          FTaskBarList.SetProgressState(AppHandle, TBPF_Normal)
        else if Widget.Paused then
          FTaskBarList.SetProgressState(AppHandle, TBPF_PAUSED)
        else
          Assert(False); // We should not come here
        FTaskBarList.SetProgressValue(AppHandle, TaskbarPercent, 100);
        {if GlobalUserConfig.TaskbarIconType = TaskbarOverlayIcon then
           FTaskBarList.SetOverlayIcon(AppHandle,
             FOverlayProgressIcons[Index + 1].Handle,
             PWideChar(''));}
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

  end;

end;

procedure TfrmMain.OptionsFormClosed();
var
  Temp: TfraTimer;
  //Count: integer;
begin

  {Once the options form is closed, tool buttons have to be set according
  to the changed options}
  with UserConfig do
  begin
    if tbProgressAuto.Down <> AutoProgress then
    begin
      tbProgressAuto.Down := AutoProgress;
      tbProgressAutoClick(Self);
    end;
    //for Count := 0 to FTimerFrames.Count - 1 do
    for Temp in FTimerFrames do
    begin
      //Temp := FTimerFrames.Data[Count];
      Temp.TitleEditable := AllowTimerTitleEdit;
    end;

    tiMain.Visible := ShowTrayIcon;
  end;

  tbVolume.Position := AudioSystem.Volume;

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
begin
  Conf := TJSONConfig.Create(nil);
  Conf.Formatted := True;
  SaveClocks(Conf);
  Conf.Filename := FDbFileName;
  Conf.Free;
end;

procedure TfrmMain.LoadfromFile;
var
  Conf: TJSONConfig;
  TotalCount, Count: integer;
  NewTimerClock: TfraTimer;
  ErrorText, AudioFileName: string;
  fs: TFormatSettings;
  State: TTimerState;
  SoundIndex: integer = 0;
  SoundType: integer = 0;
begin

  ErrorText := '';

  if FileExists(FDbFileName) then
  begin

    Conf := TJSONConfig.Create(nil);
    Conf.FileName := FDbFileName;

    TotalCount := Conf.GetValue(TIMER_CONF_COUNT, 0);

    for Count := 0 to TotalCount - 1 do
    begin
      Conf.OpenKey(UTF8Decode(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
        '/'), False);

      NewTimerClock := AddTimer;
      NewTimerClock.Caption :=
        string(Conf.GetValue(TIMER_CONF_TITLE, DEF_TIMER_TITLE));

      { When float is saved, it is saved as string }
      fs := FormatSettings;
      fs.DecimalSeparator := '.';

      NewTimerClock.Duration :=
        StrToFloat(string(Conf.GetValue(TIMER_CONF_DURATION, '0')), fs);
      NewTimerClock.IsProgressOnIcon :=
        Conf.GetValue(UTF8Decode(TIMER_CONF_NOTIFIER), False);

      AudioFileName := string(Conf.GetValue(TIMER_CONF_SOUNDFILEPATH, ''));

      SoundType := Conf.GetValue(TIMER_CONF_SOUNDTYPE, TIMER_CONF_SOUND_DEFALUT);

      if AudioSystem.Loaded then
      begin
        NewTimerClock.SoundIndex := SoundPool.DefaultSoundIndex;
        case SoundType of
          TIMER_CONF_SOUND_NONE:
            NewTimerClock.SoundIndex := INVALID_SOUNDPOOL_INDEX;
          TIMER_CONF_SOUND_DEFALUT:
            NewTimerClock.SoundIndex := SoundPool.DefaultSoundIndex;
          TIMER_CONF_SOUND_CUSTOM:
          begin
            SoundIndex := SoundPool.LoadSoundFromFile(AudioFileName);
            if SoundIndex > INVALID_SOUNDPOOL_INDEX then
            begin
              NewTimerClock.SoundIndex := SoundIndex;
              NewTimerClock.LoadedSoundIndex := SoundIndex;
              NewTimerClock.LoadedSoundSource := AudioFileName;
            end
            else
            begin
              Application.MessageBox(pansichar('Could not load file ' +
                AudioFileName + '. Restting to default sound.'), 'Alert', MB_OK);
              NewTimerClock.SoundIndex := SoundPool.DefaultSoundIndex;
            end;
          end
          else
          begin
            NewTimerClock.SoundIndex := SoundPool.DefaultSoundIndex;
            Logger.Error('Invalid sound type loaded for clock #' +
              IntToStr(Count) + ' - ' + IntToStr(SoundType) + ' at ' +
              string({$INCLUDE %FILE%}) + ':' + string({$INCLUDE %LINE%}));
          end;
        end;
        NewTimerClock.SoundLooped := Conf.GetValue(TIMER_CONF_SOUNDLOOP, False);
      end;

      NewTimerClock.ModalAlert :=
        Conf.GetValue(TIMER_CONF_MODALALERT, False);
      NewTimerClock.TrayNotification :=
        Conf.GetValue(TIMER_CONF_TRAYNOTIFICATION, False);

      NewTimerClock.Metronome := Conf.GetValue(TIMER_CONF_METRONOME, False);

      State.Running := Conf.GetValue(TIMER_CONF_RUNNING, False);
      State.Paused := Conf.GetValue(TIMER_CONF_PAUSED, False);
      State.PendingTicks :=
        StrToQWord(string(Conf.GetValue(TIMER_CONF_PENDINGTICKCOUNT, '0')));
      State.EndTime := StrToFloat(
        string(Conf.GetValue(TIMER_CONF_ENDTIME, '0')), fs);
      State.DurationTicks :=
        StrToQword(string(Conf.GetValue(TIMER_CONF_ORIGTICKCOUNT, '0')));

      NewTimerClock.LoadState(State);

      PostTimerCreation(NewTimerClock);

      Conf.CloseKey;
    end;

    Conf.Free;
    //Reorder;

    if ErrorText = '' then
      Logger.Info('Saved timers loaded.')
    else
    begin
      Logger.Info('Saved timers loaded (with errors).');
      ShowMessage(ErrorText);
    end;
    UpdateStatusTimerCount;
  end;
end;

function TfrmMain.AddTimer(): TfraTimer;
var
  Id: longword;
  NewWidget, WidgetAbove: TfraTimer;
  Index: integer;
begin
  Id := FCounterClockID.NextVal;

  NewWidget := TfraTimer.Create(pnlClocks);
  //NewWidget.Id := Id;
  NewWidget.Name := NewWidget.Name + IntToStr(Id);
  NewWidget.Parent := pnlClocks;
  NewWidget.TitleEditable := UserConfig.AllowTimerTitleEdit;

  NewWidget.imgTimer.Picture.Assign(FWidgetStoppedBitmap);
  NewWidget.LastProgressIconIndex := LAST_TRAY_ICON_DEFAULT;

  NewWidget.AnchorSide[akRight].Side := asrRight;
  NewWidget.AnchorSide[akRight].Control := pnlClocks;

  NewWidget.AnchorSide[akLeft].Side := asrLeft;
  NewWidget.AnchorSide[akLeft].Control := pnlClocks;

  //FTimerFrames.Add(Id, NewWidget);
  Index := FTimerFrames.Add(NewWidget);

  if Index = 0 then
  begin
    NewWidget.AnchorSide[akTop].Side := asrTop;
    NewWidget.AnchorSide[akTop].Control := pnlClocks;
  end
  else
  begin
    WidgetAbove := FTimerFrames.Items[Index - 1];
    NewWidget.AnchorSide[akTop].Side := asrBottom;
    NewWidget.AnchorSide[akTop].Control := WidgetAbove;
    NewWidget.Top := WidgetAbove.Top + WidgetAbove.Height;
  end;

  //FOrder.Add(Id);


  //Reorder;
  //sbxClocks.Refresh;
  pnlClocks.Refresh;
  Result := NewWidget;
end;

procedure TfrmMain.HandleTimerFrameIconProgressChange(Sender: TfraTimer);
var
  Temp: TfraTimer;
  //Count: integer;
begin
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

  { For all timers other than the sender, uncheck if checked }
  //for Count := 0 to FTimerFrames.Count - 1 do
  for Temp in FTimerFrames do
  begin
    //Temp := FTimerFrames.Data[Count];
    if Temp <> Sender then
      if Temp.IsProgressOnIcon then
      begin
        Temp.CallbackOnProgressOnIconChange := False;
        Temp.IsProgressOnIcon := False;
        Temp.CallbackOnProgressOnIconChange := True;

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
  //Id: longword;
  fs: TFormatSettings;
  State: TTimerState;
begin

  { While saving, existing IDs of clocks are ignored.
  New IDs are generated in sequence. }

  Conf.SetValue(TIMER_CONF_COUNT, FTimerFrames.Count);

  //if FOrder.Count <> FTimerFrames.Count then
  //begin
  //  ShowMessage('Fatal error - FOrder.Count does not match FTimerFrames.Count' +
  //    LineEnding + 'Saved timers will be lost.');
  //  Exit;
  //end;


  for Count := 0 to FTimerFrames.Count - 1 do
  begin
    Conf.OpenKey(UTF8Decode(TIMER_CONF_TIMERS + '/' + IntToStr(Count + 1) +
      '/'), True);

    { FOrder has the order of IDs }
    //Id := FOrder[Count];

    //TimerClock := FTimerFrames.KeyData[Id];
    TimerClock := FTimerFrames.Items[Count];

    if TimerClock = nil then
      ShowMessage('Clock is Nil');

    Conf.SetValue(UTF8Decode(TIMER_CONF_TITLE), UTF8Decode(TimerClock.Caption));

    fs := FormatSettings;
    fs.DecimalSeparator := '.';
    Conf.SetValue(UTF8Decode(TIMER_CONF_DURATION),
      UTF8Decode(FloatToStr(TimerClock.Duration, fs)));

    Conf.SetValue(UTF8Decode(TIMER_CONF_NOTIFIER), TimerClock.IsProgressOnIcon);

    Conf.SetValue(TIMER_CONF_MODALALERT,
      TimerClock.ModalAlert);
    Conf.SetValue(TIMER_CONF_TRAYNOTIFICATION,
      TimerClock.TrayNotification);

    Conf.SetValue(TIMER_CONF_SOUNDFILEPATH, '');
    if AudioSystem.Loaded then
    begin
      if TimerClock.SoundIndex = INVALID_SOUNDPOOL_INDEX then
        Conf.SetValue(TIMER_CONF_SOUNDTYPE, TIMER_CONF_SOUND_NONE)
      else if TimerClock.SoundIndex = SoundPool.DefaultSoundIndex then
        Conf.SetValue(TIMER_CONF_SOUNDTYPE, TIMER_CONF_SOUND_DEFALUT)
      else if TimerClock.SoundIndex >= SoundPool.CustomSoundRangeStart then
      begin
        Conf.SetValue(TIMER_CONF_SOUNDTYPE, TIMER_CONF_SOUND_CUSTOM);
        Conf.SetValue(TIMER_CONF_SOUNDFILEPATH,
          TimerClock.LoadedSoundSource);
      end
      else
        Logger.Warning('Error while saving clocks');
      Conf.SetValue(TIMER_CONF_SOUNDLOOP, TimerClock.SoundLooped);

    end;

    State.Paused := False;

    TimerClock.SaveState(State);

    Conf.SetValue(TIMER_CONF_RUNNING, State.Running);
    Conf.SetValue(TIMER_CONF_PAUSED, State.Paused);

    { Set pending tick count and end time to zero to begin with
    Then udpate the ones applicable afterwards. }
    Conf.SetValue(TIMER_CONF_PENDINGTICKCOUNT, IntToStr(State.PendingTicks));
    Conf.SetValue(TIMER_CONF_ENDTIME, FloatToStr(State.EndTime, fs));
    Conf.SetValue(TIMER_CONF_ORIGTICKCOUNT, IntToStr(State.DurationTicks));


    Conf.SetValue(TIMER_CONF_METRONOME, TimerClock.Metronome);

    Conf.CloseKey;
  end;

end;

procedure TfrmMain.DeleteSelected;
var
  //Id: longword;
  TimerClock: TfraTimer;
  //Count: integer;
  //IdList: TIdList;
  TobeRemoved: TTimerFrameList;
  StartTickCount: QWord;
begin
  //IdList := TIdList.Create;
  TobeRemoved := TTimerFrameList.Create;

  { FPGMap does not have an iterator. So removal of elements have to be done
  with kid-gloves. A seperate list is created to hold IDs of elements to be
  removed from the list. After iteration, removal is done in a separte loop}

  //for Count := 0 to FTimerFrames.Count - 1 do
  for TimerClock in FTimerFrames do
  begin
    //TimerClock := FTimerFrames.Data[Count];
    //if TimerClock = nil then
    //  ShowMessage('Clock is Nil');
    Assert(Assigned(TimerClock));

    //if TimerClock.Running or TimerClock.IsSoundPlaying then;
    //TimerClock.Stop(True);

    //StartTickCount := GetTickCount64;
    //{ Abort is asynchronous, wait till each timer aborts.
    //Also, we do not wait for more than two seconds per timer.
    //After that, it is past caring. Tardiness can be tolerated only as much. }
    //while TimerClock.IsSoundPlaying do
    //begin
    //  Logger.Debug('Waiting for frame ' + IntToStr(Count) + ' to stop audio');
    //  Application.ProcessMessages;
    //  if GetTickCount64 > (StartTickCount + AUDIO_ABORT_LONG_WAIT) then
    //    break;
    //end;

    if TimerClock.Selected then
    begin

      if TimerClock.Running or TimerClock.IsSoundPlaying then;
      TimerClock.Stop(True);

      StartTickCount := GetTickCount64;
      { Abort is asynchronous, wait till each timer aborts.
      Also, we do not wait for more than two seconds per timer.
      After that, it is past caring. Tardiness can be tolerated only as much. }
      while TimerClock.IsSoundPlaying do
      begin
        Logger.Debug('Waiting for frame to stop audio');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_LONG_WAIT) then
          break;
      end;

      //IdList.Add(TimerClock.Id);
      TobeRemoved.Add(TimerClock);
    end;

  end;

  //for Id in IdList do
  for TimerClock in TobeRemoved do

  begin
    RemoveTimer(TimerClock);
  end;
  //IdList.Free;
  TobeRemoved.Free;

end;

procedure TfrmMain.RemoveTimer(Widget: TfraTimer);
var
  //RemovedTimer: TfraTimer;
  Index: integer;
  NextWidget: TfraTimer;
begin
  //Index := FTimerFrames.IndexOf(IdNew);
  //RemovedTimer := TfraTimer(FTimerFrames.Data[Index]);
  //FTimerFrames.Remove(IdNew);
  Index := FTimerFrames.IndexOf(Widget);

  if Index = 0 then
  begin
    NextWidget := FTimerFrames.Items[1];
    NextWidget.AnchorSide[akTop].Side := asrTop;
    NextWidget.AnchorSide[akTop].Control := pnlClocks;
  end
  else if Index < FTimerFrames.Count - 1 then
  begin
    NextWidget := FTimerFrames.Items[Index + 1];
    NextWidget.AnchorSide[akTop].Side := asrBottom;
    NextWidget.AnchorSide[akTop].Control := Widget.AnchorSide[akTop].Control;
  end;

  FTimerFrames.Remove(Widget);
  //FOrder.Remove(IdNew);
  Widget.Free;
  //Reorder;
end;

procedure TfrmMain.MoveSelectedClocksUp;
var
  //Id: longword;
  Widget, WidgetAbove: TfraTimer;
  Count: integer;
  TempControl: TControl;
  TempReference: TAnchorSideReference;
  TempTop: integer;
begin
  //Count := 0;
  //for Id in FOrder do
  for Count := 1 to FTimerFrames.Count - 1 do
  begin
    //if Count = 0 then
    //begin
    //  Inc(Count);
    //  Continue;
    //end;
    Widget := FTimerFrames.Items[Count];
    WidgetAbove := FTimerFrames.Items[Count - 1];
    //if FTimerFrames.KeyData[Id].Selected then
    if Widget.Selected and (not WidgetAbove.Selected) then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      //if not (FTimerFrames.KeyData[FOrder.Items[Count - 1]].Selected and
      //  FTimerFrames.KeyData[FOrder.Items[Count]].Selected) then
      //  FOrder.Exchange(Count - 1, Count);
      FTimerFrames.Exchange(Count - 1, Count);

      TempReference := Widget.AnchorSide[akTop].Side;
      TempControl := Widget.AnchorSide[akTop].Control;
      TempTop := Widget.Top;

      Widget.AnchorSide[akTop].Side := WidgetAbove.AnchorSide[akTop].Side;
      Widget.AnchorSide[akTop].Control := WidgetAbove.AnchorSide[akTop].Control;
      Widget.Top := WidgetAbove.Top;

      WidgetAbove.AnchorSide[akTop].Side := TempReference;
      WidgetAbove.AnchorSide[akTop].Control := TempControl;
      WidgetAbove.Top := TempTop;

      //WidgetAbove.ReAlign;
      //Widget.ReAlign;

    end;
    //Inc(Count);
  end;
  //Reorder;
  //pnlClocks.ReAlign;
  //pnlClocks.Hide;
  //pnlClocks.Show;
  //pnlClocks.Refresh;
  //sbxClocks.Refresh;
end;

procedure TfrmMain.MoveSelectedClocksDown;
var
  //Id: longword;
  Count: integer;
  //First: boolean;
  Widget, WidgetBelow: TfraTimer;
  TempControl: TControl;
  TempReference: TAnchorSideReference;
  TempTop: integer;
begin
  //First := True;
  for Count := (FTimerFrames.Count - 2) downto 0 do
  begin
    //if First then
    //begin
    //  First := False;
    //  Continue;
    //end;
    //Id := FOrder.Items[Count];

    Widget := FTimerFrames.Items[Count];
    WidgetBelow := FTimerFrames.Items[Count + 1];

    //if FTimerFrames.KeyData[Id].Selected then
    //begin
    //  {If x and y are selected, do not exchange, keep the order as it is}
    //  if not (FTimerFrames.KeyData[FOrder.Items[Count + 1]].Selected and
    //    FTimerFrames.KeyData[FOrder.Items[Count]].Selected) then
    //    FOrder.Exchange(Count + 1, Count);
    //end;

    if Widget.Selected and (not WidgetBelow.Selected) then
    begin
      {If x and y are selected, do not exchange, keep the order as it is}
      //if not (FTimerFrames.KeyData[FOrder.Items[Count - 1]].Selected and
      //  FTimerFrames.KeyData[FOrder.Items[Count]].Selected) then
      //  FOrder.Exchange(Count - 1, Count);
      FTimerFrames.Exchange(Count, Count + 1);

      TempReference := Widget.AnchorSide[akTop].Side;
      TempControl := Widget.AnchorSide[akTop].Control;
      TempTop := Widget.Top;

      Widget.AnchorSide[akTop].Side := WidgetBelow.AnchorSide[akTop].Side;
      Widget.AnchorSide[akTop].Control := WidgetBelow.AnchorSide[akTop].Control;
      Widget.Top := WidgetBelow.Top;

      WidgetBelow.AnchorSide[akTop].Side := TempReference;
      WidgetBelow.AnchorSide[akTop].Control := TempControl;
      WidgetBelow.Top := TempTop;

      //WidgetAbove.ReAlign;
      //Widget.ReAlign;

    end;

  end;
  //Reorder;
end;

procedure TfrmMain.OnShortTimer(Sender: TObject);
var
  TimerFrame: TfraTimer;
begin
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
        Logger.Debug('Exception in TfrmMain.OnShortTimer: ' + E.ClassName +
          LineEnding + E.Message);
    end;
  finally
  end;

end;

procedure TfrmMain.AfterShow(Data: PtrInt);
var
  TimerFrame: TfraTimer;
  //Count: integer;
begin

  { If there are no timers, prompt user to add one }

  if FTimerFrames.Count = 0 then
  begin
    aiNewTimer.Execute;

    Assert(FTimerFrames.Count <= 1);

    { If a timer was added, turning reporting on by default.
    When the application is run for the first time, the user has to be made
    aware of this functionality, lest he/she remians unaware forever }

    if FTimerFrames.Count > 0 then
    begin
      with FTimerFrames.Items[0] do
      begin
        TrayNotification := True;
        ckbIconProgress.Checked := True;
      end;
    end;

  end
  else
  { If there are loaded timers which are running/paused, ask them
  to handle a timer trigger so that bitmaps are set properly. This is
  done here solely because of overlay icons, which does not seem to get
  updated before the form is shown.}
  begin
    //for Count := 0 to FTimerFrames.Count - 1 do
    for TimerFrame in FTimerFrames do
    begin
      //TimerFrame := FTimerFrames.Data[Count];
      if TimerFrame.Running then
        TimerFrame.HandleTimerTrigger;
    end;
  end;
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

procedure TfrmMain.AddAlert(Data: PtrInt);
var
  Widget: TfraTimer;
begin
  Widget := TfraTimer(Data);
  frmAlert.AddTimer(Widget);

end;


end.
