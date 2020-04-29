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
unit timerframe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, EditBtn, Dialogs, ActnList, dateutils, settings,
  editform, Graphics, Math, EventLog, adjustform, {sndfile, portaudio,} audio,
  {ctypes,} LMessages, LCLIntf, StrUtils;

const
  TIMER_IMG_GREY_TIMER: integer = 0;
  TIMER_IMG_COLOUR_TIMER: integer = 1;
  TIMER_IMG_NOTIFY_YES: integer = 2;
  TIMER_IMG_NOTIFY_NO: integer = 3;

  CLOCK_HEIGHT = 38;
  DEF_COUNTDOWN_CAPTION: string = '00:00:00';
  TIMER_PROGRESS_FINISHED: single = 2.0;

  TIMER_CONF_CLOCKS = 'clocks';
  TIMER_CONF_TIMERS = 'timers';
  TIMER_CONF_TITLE = 'timer_title';
  TIMER_CONF_TIME = 'time';
  //TIMER_CONF_HOURS = 'hours';
  //TIMER_CONF_MINUTES = 'minutes';
  //TIMER_CONF_SECONDS = 'seconds';
  TIMER_CONF_DURATION = 'duration';
  TIMER_CONF_NOTIFIER = 'notifier';
  //TIMER_CONF_ID = 'id';
  TIMER_CONF_COUNT = 'count';
  //TIMER_CONF_ORDER = 'order';

  TIMER_CONF_SOUND = 'audio_file';
  TIMER_CONF_SOUNDLENGTH = 'audio_duration';
  TIMER_CONF_SOUNDLOOP = 'audio_loop';


  TIMER_CONF_MODALALERT = 'modal_alert';
  TIMER_CONF_TRAYNOTIFICATION = 'tray_notification';

  TIMER_CONF_PENDINGTICKCOUNT = 'pending_tick_count';
  TIMER_CONF_ENDTIME = 'end_time';
  TIMER_CONF_ORIGTICKCOUNT = 'orig_tick_count';


  TIMER_CONF_USEDEFSOUND = 'use_default_audio';

  TIMER_CONF_RUNNING = 'running';
  TIMER_CONF_PAUSED = 'paused';

  TIMER_CONF_METRONOME = 'metronome';

  //UM_PLAY_AUDIO = LM_USER + 1;
  //UM_FINISHED_AUDIO = LM_USER + 2;

//BUFFER_SIZE = 1024;

type

  TTimerState = record
    Running: boolean;
    Paused: boolean;
    PendingTicks: longword;
    EndTime: TDateTime;
    DurationTicks: longword;
  end;

  { TfraTimer }

  TfraTimer = class(TFrame)
    aiAdjust: TAction;
    aiEdit: TAction;
    aiStop: TAction;
    aiPause: TAction;
    aiPlay: TAction;
    alTimer: TActionList;
    bbPlay: TBitBtn;
    bbPause: TBitBtn;
    bbEdit: TBitBtn;
    bbStop: TBitBtn;
    bbAdjust: TBitBtn;
    cbSelect: TCheckBox;
    ckbIconProgress: TCheckBox;
    dtpSet: TDateTimePicker;
    edtTitle: TEdit;
    ilTimer: TImageList;
    imgTimer: TImage;
    lblCountdown: TLabel;
    procedure aiAdjustExecute(Sender: TObject);
    procedure aiEditExecute(Sender: TObject);
    procedure aiPauseExecute(Sender: TObject);
    procedure aiPlayExecute(Sender: TObject);
    procedure aiStopExecute(Sender: TObject);
    procedure bbAdjustClick(Sender: TObject);
    procedure ckbIconProgressChange(Sender: TObject);
    procedure dtpSetChange(Sender: TObject);
    procedure imgTimerClick(Sender: TObject);

  private
    { private declarations }
    FId: longword;
    AudioCriticalSection: TRTLCriticalSection;

    FModalAlert: boolean;
    FTrayNotification: boolean;
    FTitleEditable: boolean;

    FOrigTickDuration: longword;
    FStartTickCount: longword;
    FEndTickCount: longword;
    FPendingTickCount: longword;

    FPaused: boolean;
    FRunning: boolean;
    FProgress: single;

    FAudio: TAudio;
    FDefaultSound: TSound;
    FCustomSound: TSound;

    //procedure SetAudio(AValue: TAudio);
    function GetIsSoundPlaying: boolean;
    procedure SetCustomSound(AValue: TSound);
    procedure SetId(AValue: longword);
    function GetCaption: string;
    function GetCounter: string;
    function GetDuration: TDateTime;
    function GetDurationEnabled: boolean;
    function GetIsProgressOnIcon: boolean;
    function GetPauseButtonEnabled: boolean;
    function GetPlayButtonEnabled: boolean;
    function GetSelected: boolean;
    function GetStopButtonEnabled: boolean;
    procedure SetCaption(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    procedure SetIsProgressOnIcon(AValue: boolean);
    procedure SetMetronome(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);

    procedure SetStopButtonEnabled(AValue: boolean);
    function GetId: longword;
    procedure SetTitleEditable(AValue: boolean);
    procedure SetTrayNotification(AValue: boolean);
    procedure UpdateProgress(const PendingMilliseconds: longword);
    procedure ReenableEditControls;

  public
    { public declarations }

    LastProgressIconIndex: integer;
    UseDefaultSound: boolean;

    SoundInfo: TTimerSoundInfo;
    SoundLooped: boolean;

    FMetronome: boolean;

    // Callback on progress-on-icon checkbox change only if
    // this variable is true. Used to avoid unending triggering of events.
    CallbackOnProgressOnIconChange: boolean;

    procedure ClockSelected(Sender: TObject);
    procedure AudioPlayed(Sender: TObject);
    procedure Hide;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Id: longword read FId write SetId;
    procedure SetCounter(AValue: string);
    procedure CheckForZeroTime;

    procedure HandleTimerTrigger();
    procedure Start;
    procedure Pause;
    procedure Stop(UserInitiated: boolean);

    procedure Finish;
    procedure PublishProgress(Percent: single);

    procedure AdjustTimer(Sender: TObject);

    procedure SaveState(var SaveTo: TTimerState);
    procedure LoadState(var LoadFrom: TTimerState);

    procedure AbortSound;

    property PlayButtonEnabled: boolean read GetPlayButtonEnabled
      write SetPlayButtonEnabled;
    property PauseButtonEnabled: boolean read GetPauseButtonEnabled
      write SetPauseButtonEnabled;
    property StopButtonEnabled: boolean read GetStopButtonEnabled
      write SetStopButtonEnabled;
    property Counter: string read GetCounter write SetCounter;
    property Duration: TDateTime read GetDuration write SetDuration;
    property DurationEnabled: boolean read GetDurationEnabled write SetDurationEnabled;
    property Caption: string read GetCaption write SetCaption;

    property IsProgressOnIcon: boolean read GetIsProgressOnIcon
      write SetIsProgressOnIcon;

    property IsSoundPlaying: boolean read GetIsSoundPlaying;

    property Selected: boolean read GetSelected;

    property Running: boolean read FRunning;
    property Paused: boolean read FPaused;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;
    property TrayNotification: boolean read FTrayNotification write SetTrayNotification;
    property TitleEditable: boolean read FTitleEditable write SetTitleEditable;
    property Progress: single read FProgress;
    //property Audio: TAudio read FAudio;// write SetAudio;
    property CustomSound: TSound read FCustomSound write SetCustomSound;
    property Metronome: boolean read FMetronome write SetMetronome;
    //property PendingTickCount: longword read FPendingTickCount;

  end;

implementation

uses
  main;

{$R *.lfm}

{ TfraTimer }

procedure TfraTimer.dtpSetChange(Sender: TObject);
begin
  CheckForZeroTime;
end;

procedure TfraTimer.aiPlayExecute(Sender: TObject);
begin
  Start;
end;

procedure TfraTimer.aiStopExecute(Sender: TObject);
begin
  Stop(True);
end;

procedure TfraTimer.bbAdjustClick(Sender: TObject);
begin

end;

procedure TfraTimer.ckbIconProgressChange(Sender: TObject);
begin
  if CallbackOnProgressOnIconChange then
  begin
    frmMain.HandleTimerFrameIconProgressChange(Self);
    {We have a special case if the timer is in paused state. In this case
    timer events are not triggered for this timer. Manually trigger one so that
    the icons are updated. }
    if ckbIconProgress.Checked and FPaused then
      frmMain.ProgressUpdate(Self, FProgress);
  end;
end;

procedure TfraTimer.aiPauseExecute(Sender: TObject);
begin
  Pause;
end;

procedure TfraTimer.aiEditExecute(Sender: TObject);
{var
  NewCustomSound: TSound = nil;
  OldCustomSound: TSound = nil; }
begin
  frmEdit.Description := edtTitle.Text;
  frmEdit.Duration := dtpSet.Time;
  frmEdit.TrayNotification := FTrayNotification;
  frmEdit.ModalAlert := FModalAlert;
  frmEdit.UseDefaultSound:=UseDefaultSound;

  if TAudio.Loaded then
  begin
    frmEdit.CurrentSound := FCustomSound;
  end
  else
  begin
    frmEdit.CurrentSound := Nil;
    frmEdit.edtSound.Text:= SoundInfo.FileName;
    frmEdit.SoundDuration:= SoundInfo.Duration;
  end;

  frmEdit.SoundLooped:=SoundLooped;
  frmEdit.Metronome:=Metronome;

  if frmEdit.ShowForEdit(Self) then
  begin
    Caption := frmEdit.Description;
    dtpSet.Time := frmEdit.Duration;
    FTrayNotification := frmEdit.TrayNotification;
    UseDefaultSound:=frmEdit.UseDefaultSound;
    FModalAlert := frmEdit.ModalAlert;

    //OldCustomSound := FCustomSound;

    SoundLooped:=frmEdit.SoundLooped;
    Metronome:=frmEdit.Metronome;
    if TAudio.Loaded then
    begin
      //frmEdit.SoundLooped:=  frmEdit.ckbLoop.Checked;
      //NewCustomSound := frmEdit.NewSound;
      if frmEdit.NewSound <> nil then
      begin
        FCustomSound.Free;
        //OldCustomSound := nil;
        FCustomSound := frmEdit.NewSound;
        { Now what we have taken over the responsibility of the new sound
        created, we will set it to nil for frmEdit }
        frmEdit.NewSound := nil;
      end;

    end
    else
    begin
      SoundInfo.FileName := frmEdit.edtSound.Text;
      SoundInfo.Duration := frmEdit.SoundDuration;
      SoundInfo.Looped := frmEdit.ckbLoop.Checked;
    end;
    frmMain.SavetoFile;
  end;
end;

procedure TfraTimer.aiAdjustExecute(Sender: TObject);
begin
  frmAdjust.OnAdjust := @AdjustTimer;
  if not FPaused then
  begin
    if frmAdjust.cmbOptions.Items.Count <= 2 then
      frmAdjust.cmbOptions.Items.Add(ADJUST_STOPBY_TEXT);
  end
  else
  begin
    if frmAdjust.cmbOptions.Items.Count = 3 then
      frmAdjust.cmbOptions.Items.Delete(ADJUST_STOPBY);
  end;
  frmAdjust.cmbOptions.ItemIndex := 0;
  frmAdjust.bbApply.Enabled := True;
  frmAdjust.dtpDiff.Show;
  frmAdjust.dtpTill.Hide;
  frmAdjust.Id := FId;
  frmAdjust.ShowModal;
end;


procedure TfraTimer.imgTimerClick(Sender: TObject);
begin

end;

procedure TfraTimer.SetId(AValue: longword);
begin
  Assert(AValue > 0);
  if FId > 0 then
    Exit;
  if FId = AValue then
    Exit;
  FId := AValue;
  Name := Name + IntToStr(AValue);
end;

{procedure TfraTimer.SetAudio(AValue: TAudio);
begin
  FAudio.Free;
  FAudio := AValue
end;}

procedure TfraTimer.SetCustomSound(AValue: TSound);
var
  OldCustomSound: TSound;
begin
  if FCustomSound=AValue then Exit;

  //OldCustomSound := FCustomSound;
  //FCustomSound:=AValue;

  //OldCustomSound.Free;
  FCustomSound.Free;
  FCustomSound:= AValue;

end;

function TfraTimer.GetIsSoundPlaying: boolean;
begin
  Result := FAudio.Playing;
end;

function TfraTimer.GetCaption: string;
begin
  Result := edtTitle.Text;
end;

function TfraTimer.GetCounter: string;
begin
  Result := lblCountdown.Caption;
end;

function TfraTimer.GetDuration: TDateTime;
begin
  Result := dtpSet.Time;
end;

function TfraTimer.GetDurationEnabled: boolean;
begin
  Result := dtpSet.Enabled;
end;

function TfraTimer.GetIsProgressOnIcon: boolean;
begin
  Result := ckbIconProgress.Checked;
end;

function TfraTimer.GetPauseButtonEnabled: boolean;
begin
  Result := bbPause.Enabled;
end;

function TfraTimer.GetPlayButtonEnabled: boolean;
begin
  Result := bbPlay.Enabled;
end;

function TfraTimer.GetSelected: boolean;
begin
  Result := cbSelect.Checked;
end;

function TfraTimer.GetStopButtonEnabled: boolean;
begin
  Result := bbStop.Enabled;
end;

procedure TfraTimer.SetCaption(AValue: string);
begin
  edtTitle.Text := AValue;
end;

procedure TfraTimer.SetDuration(AValue: TDateTime);
begin
  dtpSet.Time := AValue;
  CheckForZeroTime;
end;

procedure TfraTimer.SetDurationEnabled(AValue: boolean);
begin
  dtpSet.Enabled := AValue;
end;

procedure TfraTimer.SetIsProgressOnIcon(AValue: boolean);
begin
  ckbIconProgress.Checked := AValue;
end;

procedure TfraTimer.SetMetronome(AValue: boolean);
begin
  if FMetronome=AValue then Exit;
  FMetronome:=AValue;
  if Running then
    if FMetronome then
      frmMain.Metronome.Subscribe
    else
      frmMain.Metronome.Unsubscribe;
end;

procedure TfraTimer.SetModalAlert(AValue: boolean);
begin
  if FModalAlert = AValue then
    Exit;
  FModalAlert := AValue;
end;

procedure TfraTimer.SetPauseButtonEnabled(AValue: boolean);
begin
  bbPause.Enabled := AValue;
end;

procedure TfraTimer.SetPlayButtonEnabled(AValue: boolean);
begin
  bbPlay.Enabled := AValue;
end;

procedure TfraTimer.SetStopButtonEnabled(AValue: boolean);
begin
  bbStop.Enabled := AValue;
end;

function TfraTimer.GetId: longword;
begin
  Result := FId;
end;

procedure TfraTimer.SetTitleEditable(AValue: boolean);
begin
  FTitleEditable := AValue;
  edtTitle.ReadOnly := (not AValue);
  if Avalue then
    edtTitle.Color := clDefault
  else
    edtTitle.Color := clForm;
end;

procedure TfraTimer.SetTrayNotification(AValue: boolean);
begin
  if FTrayNotification = AValue then
    Exit;
  FTrayNotification := AValue;
end;

procedure TfraTimer.UpdateProgress(const PendingMilliseconds: longword);
var
  ProgressPercentage: single;
  CounterText: string;
  Seconds: integer;
  Minutes: integer;
  Hours: integer;
  Elapsed: longword;
begin
  Elapsed := Ceil(PendingMilliseconds / 1000);
  //Logger.Debug('Elapsed in ms is ' + IntToStr(PendingMilliseconds) + ' of ' + IntToStr(FOrigTickDuration));

  Seconds := Elapsed mod 60;
  Minutes := Elapsed div 60;
  Hours := Minutes div 60;
  Minutes := Minutes mod 60;
  CounterText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  if Counter <> CounterText then
    Counter := CounterText;

  // Calculate percentage ProgressPercentage

  Assert(FOrigTickDuration > 0);

  ProgressPercentage := 1 - (PendingMilliseconds / FOrigTickDuration);
  // Elapsed time can exceed total pending tick duration, in certain cases.
  // The system could go on sleep mode while a timer is running
  // (and while the timer event is being processed) and on
  // waking up, the pre-caculated tick duration could have already been
  // overshot. If that is the case, mark ProgressPercentage as zero, and the next
  // timer event will mark it as completed.
  if ProgressPercentage < 0 then
    ProgressPercentage := 0;
  Assert(ProgressPercentage <= 1);
  PUblishProgress(1 - (PendingMilliseconds / FOrigTickDuration));

end;

procedure TfraTimer.ReenableEditControls;
begin
  { When a timer stops, check if the same timer was being edited?
  If so, a few controls which were disabled can be re-enabled }
  if frmEdit.Showing and (frmEdit.Id = FId) then
  begin
    {with frmEdit do
    begin
      dtpDuration.Enabled := True;
      //frmEdit.tsAudio.Enabled:=True;
      ckbUseDefaultSound.Enabled:=True;
      bbSelectSound.Enabled:=True and (not ckbUseDefaultSound.Checked);
      bbClearSound.Enabled:=True and (not ckbUseDefaultSound.Checked);
      // Looped is enabled irrespective of whether default CurrentSound is
      // used or not.
      ckbLoop.Enabled:=True;
    end;}
    frmEdit.ReenableControls;
  end;
end;

procedure TfraTimer.ClockSelected(Sender: TObject);
begin
  frmMain.ClockSelected(Self);
end;

procedure TfraTimer.AudioPlayed(Sender: TObject);
begin
  PlayButtonEnabled := True;
  PauseButtonEnabled := False;
  StopButtonEnabled := False;
  DurationEnabled := True;

  bbAdjust.Enabled := False;

  { When a timer stops, check if the same timer was being edited?
  If so, a few controls which were disabled can be re-enabled }
  {if frmEdit.Showing and (frmEdit.Id = FId) then
  begin
    with frmEdit do
    begin
      dtpDuration.Enabled := True;
      //frmEdit.tsAudio.Enabled:=True;
      ckbUseDefaultSound.Enabled:=True;
      bbSelectAudioFile.Enabled:=True and (not ckbUseDefaultSound.Checked);
      bbClearAudioFile.Enabled:=True and (not ckbUseDefaultSound.Checked);
      // Looped is enabled irrespective of whether default sound is
      // used or not.
      ckbLoop.Enabled:=True;
    end;
  end;}
  ReenableEditControls;

  //bbEdit.Enabled := True;
end;

procedure TfraTimer.Hide;
begin
  Top := Top + Height;
end;

constructor TfraTimer.Create(AOwner: TComponent);
var
  SndFile: TSndSound;
begin
  inherited Create(AOwner);
  InitCriticalSection(AudioCriticalSection);
  FId := 0;
  FProgress := 0.0;

  with GlobalUserConfig do
  begin
    dtpSet.Time := DefaultTimerDuration;
    edtTitle.Text := DefaultTimerTitle;
  end;

  Parent := TWinControl(AOwner);

  cbSelect.OnChange := @ClockSelected;

  UseDefaultSound:=True;

  FRunning := False;
  FPaused := False;

  bbPlay.Caption := '';
  bbPause.Caption := '';
  bbStop.Caption := '';
  bbAdjust.Caption := '';
  bbAdjust.Enabled := False;
  bbEdit.Caption := '';

  bbPlay.DoubleBuffered := True;
  bbPause.DoubleBuffered := True;
  bbStop.DoubleBuffered := True;
  bbAdjust.DoubleBuffered := True;
  bbEdit.DoubleBuffered := True;
  DoubleBuffered := True;

  CallbackOnProgressOnIconChange := True;

  FAudio := Nil;
  if TAudio.Loaded then
  begin
    FAudio := TAudio.Create;
    FAudio.OnPlayCompletion:=@AudioPlayed;

    SndFile := TSndSound.Create;
    SndFile.LoadDefaultSound;
    FDefaultSound := SndFile;
  end;
  FCustomSound := nil;
  Metronome:=False;
end;

destructor TfraTimer.Destroy;
begin
  FAudio.Free;
  FDefaultSound.Free;
  FCustomSound.Free;
  Parent := nil;

  //DoneCriticalsection(AudioCriticalSection);
  inherited Destroy;
end;

procedure TfraTimer.SetCounter(AValue: string);
begin
  lblCountdown.Caption := AValue;
end;

procedure TfraTimer.CheckForZeroTime;
var
  TempDuration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
begin
  TempDuration := dtpSet.Time;
  Hours := HourOf(TempDuration);
  Minutes := MinuteOf(TempDuration);
  Seconds := SecondOf(TempDuration);
  // If Hours, Minutes, Seconds, all are zero then disable play button
  bbPlay.Enabled := not ((Hours = 0) and (Minutes = 0) and (Seconds = 0));
end;

procedure TfraTimer.HandleTimerTrigger();
var
  PendingMilliseconds: longword;
  CurrTickCount: longword;
begin
  EnterCriticalSection(AudioCriticalSection);

  if FRunning then
  begin
    if not FPaused then
    begin
      CurrTickCount := GetTickCount64;

      if FEndTickCount <= CurrTickCount then
      begin
        Counter := DEF_COUNTDOWN_CAPTION;
        Finish;
        LeaveCriticalSection(AudioCriticalSection);
        Exit;
      end;
      PendingMilliseconds := FEndTickCount - CurrTickCount;
    end
    else
      { Generally, there is no timer triggered when a timer is paused,
      except when the timer is loaded from file.}
      PendingMilliSeconds := FPendingTickCount;

    UpdateProgress(PendingMilliseconds);
  end;
  LeaveCriticalSection(AudioCriticalSection);
end;

procedure TfraTimer.Start;
var
  TempDuration: TDateTime;
  Hours: word;
  Minutes: word;
  Seconds: word;
begin
  TempDuration := Duration;

  Hours := HourOf(TempDuration);
  Minutes := MinuteOf(TempDuration);
  Seconds := SecondOf(TempDuration);

  if (Hours = 0) and (Minutes = 0) and (Seconds = 0) then
    Exit;

  if FPaused = False then
  begin
    FStartTickCount := GetTickCount64;

    FEndTickCount :=
      FStartTickCount + (((3600 * longword(Hours)) + (60 * longword(Minutes)) +
      longword(Seconds)) * 1000);

    FOrigTickDuration := FEndTickCount - FStartTickCount;
    if GlobalUserConfig.AutoProgress = True then
    begin
      IsProgressOnIcon := True;
    end;
  end
  else
  begin
    FStartTickCount := GetTickCount64;
    FEndTickCount := FStartTickCount + FPendingTickCount;
  end;

  FRunning := True;
  FPaused := False;

  PlayButtonEnabled := False;
  PauseButtonEnabled := True;
  StopButtonEnabled := True;
  DurationEnabled := False;

  bbAdjust.Enabled := True;
  //bbEdit.Enabled:=False;

  frmMain.TimerStarted(Self);

  if Metronome then
    frmMain.Metronome.Subscribe;

  {if frmEdit.Showing and (frmEdit.Id = FId) then
  begin
    with frmEdit do
    begin
      //dtpDuration.Enabled := False;
      //frmEdit.tsAudio.Enabled:=True;
      ckbUseDefaultSound.Enabled:=False;
      bbSelectAudioFile.Enabled:=False;
      bbClearAudioFile.Enabled:=False;
      ckbLoop.Enabled:=False;
    end;
  end;}

end;

procedure TfraTimer.Pause;
begin

  FPendingTickCount := FEndTickCount - GetTickCount64;
  if FPendingTickCount <= 0 then
    Finish;

  FPaused := True;

  PlayButtonEnabled := True;
  PauseButtonEnabled := False;
  StopButtonEnabled := True;
  DurationEnabled := False;

  frmMain.TimerPaused(Self);

  if Metronome then
    frmMain.Metronome.Unsubscribe;

end;

procedure TfraTimer.Stop(UserInitiated: boolean);
begin
  { The audio is playing and the user request is to terminate the audio.}
  Logger.Debug('Entering Stop. UserInitiated - ' + IfThen(UserInitiated,'True','False'));
  if TAudio.Loaded and FAudio.Playing then
  begin
    FRunning := False;
    FPaused := False;
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := False;
    DurationEnabled := True;

    bbAdjust.Enabled := False;
    //bbEdit.Enabled:=True;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}
    FAudio.Abort;

    Exit;
  end
  else
    {The timer run has completed. Stop the timer and play audio if required}
  begin

    // Stop the Metronome if it is running
    if Metronome and not Paused then
      frmMain.Metronome.Unsubscribe;

    FRunning := False;
    FPaused := False;
    Counter := DEF_COUNTDOWN_CAPTION;
    bbAdjust.Enabled := False;

    PublishProgress(TIMER_PROGRESS_FINISHED);

    FEndTickCount := 0;
    FOrigTickDuration := 0;
    FStartTickCount := 0;

    if frmAdjust.Showing and (frmAdjust.Id = Fid) then
      frmAdjust.bbApply.Enabled := False;

    PauseButtonEnabled := False;
    DurationEnabled := True;


    if TAudio.Loaded and (UseDefaultSound or (FCustomSound <> nil)) and (not UserInitiated) then
    begin
      PlayButtonEnabled := False;
      StopButtonEnabled := True;

      TAudio.UseDefaultDevice:=GlobalUserConfig.UseDefaultAudioDevice;

      if not TAudio.UseDefaultDevice then
      begin
        if (GlobalUserConfig.AudioDeviceName = '') or
          (GlobalUserConfig.AudioHostAPIName = '') then
          TAudio.SetDefaulDevice
        else
        begin
          TAudio.FOutputDevice.DeviceName:=GlobalUserConfig.AudioDeviceName;
          TAudio.FOutputDevice.HostAPIName:=GlobalUserConfig.AudioHostAPIName;
        end;

      end;

      // Play the sound as per configuration
      if UseDefaultSound then
        FAudio.Play(FDefaultSound, SoundLooped, GlobalUserConfig.Volume)
      else
      begin
        // If UseDefaultSound is false, then audio is loaded.
        // This is alredy checked. No need to check ...
        if FCustomSound <> nil then
          FAudio.Play(FCustomSound, SoundLooped, GlobalUserConfig.Volume);
      end;
      ReenableEditControls;
      //Logger.Debug('FAudio.Play');
    end
    else
    begin
      PlayButtonEnabled := True;
      StopButtonEnabled := False;
      { When a timer stops, check if the same timer was being edited?
      If so, a few controls which were disabled can be re-enabled }
      {if frmEdit.Showing and (frmEdit.Id = FId) then
      begin
        with frmEdit do
        begin
          dtpDuration.Enabled := True;
          //frmEdit.tsAudio.Enabled:=True;
          ckbUseDefaultSound.Enabled:=True;
          bbSelectAudioFile.Enabled:=True and (not ckbUseDefaultSound.Checked);
          bbClearAudioFile.Enabled:=True and (not ckbUseDefaultSound.Checked);
          ckbLoop.Enabled:=True;
        end;
      end; }
      ReenableEditControls;
      //bbEdit.Enabled:=True;
    end;
  end;
  frmMain.TimerFinished(Self, UserInitiated);
end;

procedure TfraTimer.Finish;
begin

  Stop(False);

  if IsProgressOnIcon then
    PublishProgress(TIMER_PROGRESS_FINISHED);

end;

procedure TfraTimer.PublishProgress(Percent: single);
begin
  {Save only actual percentage progress, or finished states.}
  if Percent <= (TIMER_PROGRESS_OFFTRAY - 0.01) then
    FProgress := Percent;

  frmMain.ProgressUpdate(Self, Percent);

end;

procedure TfraTimer.AdjustTimer(Sender: TObject);
var
  Hours, Mins, Secs: word;
  NewEndTickCount, NewPendingTickCount, CurrTickCount, Adjustment: longword;
  Diff: int64;
  EndTime: TDateTime;
begin

  // If the timer is not running, there is nothing to do
  // We should not even be in such a situation, but that is a different
  if not FRunning then
  begin
    frmAdjust.Close;
    Exit;
  end;

  case frmAdjust.cmbOptions.ItemIndex of
    ADJUST_SHORTEN:
    begin
      Hours := HourOf(frmAdjust.dtpDiff.Time);
      Mins := MinuteOf(frmAdjust.dtpDiff.Time);
      Secs := SecondOf(frmAdjust.dtpDiff.Time);

      if (Hours = 0) and (Mins = 0) and (Secs = 0) then
      begin
        frmAdjust.Close;
        Exit;
      end;

      Adjustment := (((3600 * longword(Hours)) + (60 * longword(Mins)) +
        longword(Secs)) * 1000);
      if FPaused = False then
      begin
        if Adjustment > FEndTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;
          // Adjustment is too large and cannot be used for computations
          // As a circumvention, set NewEndTickCount to 0;
          NewEndTickCount := 0;
        end
        else
        begin
          NewEndTickCount := FEndTickCount - Adjustment;
          CurrTickCount := GetTickCount64;
          if CurrTickCount >= NewEndTickCount then
          begin
            if MessageDlg('Confirm', 'This will stop the timer',
              mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
              Exit;
          end;
        end;
        FEndTickCount := NewEndTickCount;
        HandleTimerTrigger();
      end
      else  // If paused
      begin
        if Adjustment > FPendingTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;
          // Adjustment is too large and cannot be used for computations
          // As a circumvention, set NewEndTickCount to 0;
          // NewPendingTickCount := 0;
          Stop(True);
        end
        else
        begin
          //FPendingTickCount := NewPendingTickCount;
          FPendingTickCount := FPendingTickCount - Adjustment;
          //FPendingTickCount := NewPendingTickCount;
          UpdateProgress(FPendingTickCount);
        end;

      end;
      frmAdjust.Close;
    end;
    ADJUST_EXTEND:
    begin
      Hours := HourOf(frmAdjust.dtpDiff.Time);
      Mins := MinuteOf(frmAdjust.dtpDiff.Time);
      Secs := SecondOf(frmAdjust.dtpDiff.Time);

      if (Hours = 0) and (Mins = 0) and (Secs = 0) then
      begin
        frmAdjust.Close;
        Exit;
      end;

      Adjustment := (((3600 * longword(Hours)) + (60 * longword(Mins)) +
        longword(Secs)) * 1000);
      Inc(FOrigTickDuration, Adjustment);
      if FPaused = False then
      begin
        Inc(FEndTickCount, Adjustment);
        HandleTimerTrigger();
      end
      else
      begin
        Inc(FPendingTickCount, Adjustment);
        UpdateProgress(FPendingTickCount);
      end;
      frmAdjust.Close;
    end;
    ADJUST_STOPBY:
    begin
      CurrTickCount := GetTickCount64;
      if CurrTickCount >= FEndTickCount then
        Exit;
      // No need for UTC, as this is base don local TZ time selected by user
      EndTime := IncMilliSecond(Now, (FEndTickCount - CurrTickCount));

      // If extension
      if EndTime < frmAdjust.dtpTill.DateTime then
      begin
        Diff := MilliSecondsBetween(EndTime, frmAdjust.dtpTill.DateTime);
        Inc(FEndTickCount, Diff);
        Inc(FOrigTickDuration, Diff);
      end
      else // If shortening
      begin
        Diff := MilliSecondsBetween(frmAdjust.dtpTill.DateTime, EndTime);
        NewEndTickCount := FEndTickCount - Diff;
        if NewEndTickCount <= CurrTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
          begin
            Exit;
          end;
        end;
        FEndTickCount := NewEndTickCount;
      end;
      HandleTimerTrigger();
      frmAdjust.Close;
    end;
  end;
end;

procedure TfraTimer.SaveState(var SaveTo: TTimerState);
begin
  with SaveTo do
  begin
    Running := FRunning;
    Paused := FPaused;
    PendingTicks:=FPendingTickCount;
    // FPendingTickCount is reliable only if the timer is paused
    EndTime:=IncMilliSecond(LocalTimeToUniversal(Now), FEndTickCount - GetTickCount64);
    DurationTicks:=FOrigTickDuration;
  end;
end;

procedure TfraTimer.LoadState(var LoadFrom: TTimerState);
var
  NewPendingTickCount: longword;
  TimeNow: TDateTime;
begin
  { If any of the timers saved as running/paused re-instate the same state}
  if LoadFrom.Running and (not FRunning) then
  begin
    if LoadFrom.Paused then
    begin
      Pause;
      with LoadFrom do
      begin
        FRunning := Running;
        FPaused := Paused;
        FPendingTickCount:=PendingTicks;
        FOrigTickDuration:=DurationTicks;
      end;
      //UpdateProgress(FPendingTickCount);
    end
    else
    begin
      // Now moves as the processer executes. Save it in a variable.
      TimeNow:=LocalTimeToUniversal(Now);
      if LoadFrom.EndTime <= TimeNow then
      begin

        frmMain.TimerFinished(Self, False);
      end
      else
      begin
        NewPendingTickCount:=MilliSecondsBetween(TimeNow, LoadFrom.EndTime);
        Start;
        with LoadFrom do
        begin
          FRunning := Running;
          FPaused := Paused;
          FPendingTickCount:=NewPendingTickCount;
          FEndTickCount:=GetTickCount64 + NewPendingTickCount;
          FOrigTickDuration:=DurationTicks;
        end;
        //UpdateProgress(NewPendingTickCount);
      end;
    end;
  end;
end;

procedure TfraTimer.AbortSound;
begin
  FAudio.Abort;
end;


end.
