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
unit timerframe;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, ActnList, dateutils, settings,
  editform, Graphics, Math, {EventLog,} adjustform, {sndfile, portaudio,} audio,
  {ctypes,} LCLIntf, StrUtils, log{, sound}, metronome, fgl, constants, util;

type

  TTimerState = record
    Running: boolean;
    Paused: boolean;
    PendingTicks: QWord;
    EndTime: TDateTime;
    DurationTicks: QWord;
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
    //FId: longword;
    AudioCriticalSection: TRTLCriticalSection;

    FModalAlert: boolean;
    FTrayNotification: boolean;
    FTitleEditable: boolean;

    FOrigTickDuration: QWord;
    FStartTickCount: QWord;
    FEndTickCount: QWord;
    FPendingTickCount: QWord;

    FPaused: boolean;
    FRunning: boolean;
    FProgress: single;

    FAudioPlayer: TAudioPlayer;
    FSoundIndex: integer;
    FLoadedSoundIndex: integer;
    FLoadedSoundSource: string;

    FLastCompletionTime: TDateTime;

    FBeingEdited: boolean;
    FBeingAdjusted: boolean;

    function GetIsSoundPlaying: boolean;
    //procedure SetId(AValue: longword);
    function GetCaption: string;
    function GetCounter: string;
    function GetDuration: TDateTime;
    function GetDurationEnabled: boolean;
    function GetIsProgressOnIcon: boolean;
    function GetPauseButtonEnabled: boolean;
    function GetPlayButtonEnabled: boolean;
    function GetSelected: boolean;
    function GetStopButtonEnabled: boolean;
    procedure LayoutControls;
    procedure SetCaption(AValue: string);
    procedure SetDuration(AValue: TDateTime);
    procedure SetDurationEnabled(AValue: boolean);
    procedure SetIsProgressOnIcon(AValue: boolean);
    procedure SetLoadedSoundIndex(AValue: integer);
    procedure SetMetronome(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    procedure SetPauseButtonEnabled(AValue: boolean);
    procedure SetPlayButtonEnabled(AValue: boolean);
    procedure SetSoundIndex(AValue: integer);

    procedure SetStopButtonEnabled(AValue: boolean);
    //function GetId: longword;
    procedure SetTitleEditable(AValue: boolean);
    procedure SetTrayNotification(AValue: boolean);
    procedure UpdateProgress(const PendingMilliseconds: QWord);
    procedure ReenableEditControls;

  public
    { public declarations }

    LastProgressIconIndex: integer;
    SoundLooped: boolean;

    FMetronome: boolean;

    { Callback on progress-on-icon checkbox change only if
    this variable is true. Used to avoid unending triggering of events. }
    CallbackOnProgressOnIconChange: boolean;

    procedure ClockSelected(Sender: TObject);
    procedure AudioPlayed(Sender: TObject);
    procedure Hide;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //property Id: longword read FId write SetId;
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

    //procedure ArrangeControls;

    procedure AbortSound(Wait: boolean = False);

    function QueryRestartFromLastFinish: boolean;
    function RestartFromLastFinish: boolean;

    property PlayButtonEnabled: boolean read GetPlayButtonEnabled
      write SetPlayButtonEnabled;
    property PauseButtonEnabled: boolean read GetPauseButtonEnabled
      write SetPauseButtonEnabled;
    property StopButtonEnabled: boolean read GetStopButtonEnabled
      write SetStopButtonEnabled;
    property Counter: string read GetCounter write SetCounter;
    property Duration: TDateTime read GetDuration write SetDuration;
    property LastCompletedAt: TDateTime read FLastCompletionTime;
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
    property Metronome: boolean read FMetronome write SetMetronome;
    property SoundIndex: integer read FSoundIndex write SetSoundIndex;
    property LoadedSoundIndex: integer read FLoadedSoundIndex write SetLoadedSoundIndex;
    property LoadedSoundSource: string read FLoadedSoundSource write FLoadedSoundSource;
  end;

  TTimerFrameList = specialize TFPGList<TfraTimer>;

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
  ;
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

  frmEdit.SoundIndex := FSoundIndex;
  frmEdit.LoadedSoundIndex := FLoadedSoundIndex;
  frmEdit.LoadedSoundSource := FLoadedSoundSource;

  frmEdit.SoundLooped := SoundLooped;
  frmEdit.Metronome := Metronome;

  FBeingEdited := True;

  if frmEdit.ShowForEdit(Self) then
  begin

    Caption := frmEdit.Description;
    dtpSet.Time := frmEdit.Duration;
    FTrayNotification := frmEdit.TrayNotification;
    FSoundIndex := frmEdit.SoundIndex;
    FLoadedSoundIndex := frmEdit.LoadedSoundIndex;
    FLoadedSoundSource := frmEdit.LoadedSoundSource;

    FModalAlert := frmEdit.ModalAlert;

    SoundLooped := frmEdit.SoundLooped;
    Metronome := frmEdit.Metronome;

    frmMain.SavetoFile;
  end;

  FBeingEdited := False;
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
  //frmAdjust.Id := FId;
  FBeingAdjusted := True;
  frmAdjust.ShowModal;
  FBeingAdjusted := False;
end;


procedure TfraTimer.imgTimerClick(Sender: TObject);
begin
  ;
end;

//procedure TfraTimer.SetId(AValue: longword);
//begin
//  Assert(AValue > 0);
//  if FId > 0 then
//    Exit;
//  if FId = AValue then
//    Exit;
//  FId := AValue;
//  Name := Name + IntToStr(AValue);
//end;

function TfraTimer.GetIsSoundPlaying: boolean;
begin
  if AudioSystem.Loaded then
    Result := FAudioPlayer.Playing
  else
    Result := False;
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

procedure TfraTimer.LayoutControls;
begin
  { Space controls }
  //BorderSpacing.InnerBorder:= UserInterfaceMetrics.Padding;
  bbEdit.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  bbEdit.BorderSpacing.Top := (UserInterfaceMetrics.Padding div 2);

  ckbIconProgress.BorderSpacing.Right :=
    REPORT_PADDING_MULTIPLIER * UserInterfaceMetrics.Padding;
  lblCountdown.BorderSpacing.Right :=
    REPORT_PADDING_MULTIPLIER * UserInterfaceMetrics.Padding;

  bbAdjust.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  bbStop.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  bbPause.BorderSpacing.Right := UserInterfaceMetrics.Padding;
  bbPlay.BorderSpacing.Right := UserInterfaceMetrics.Padding;

  dtpSet.BorderSpacing.Right := UserInterfaceMetrics.Padding;

  edtTitle.BorderSpacing.Right := UserInterfaceMetrics.Padding;

  { Additional padding to the right of the image timer as it looks huddled
  otherwise }
  edtTitle.BorderSpacing.Left := UserInterfaceMetrics.Padding * 2;

  imgTimer.BorderSpacing.Left := UserInterfaceMetrics.Padding;
  cbSelect.BorderSpacing.Left := UserInterfaceMetrics.Padding;
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

procedure TfraTimer.SetLoadedSoundIndex(AValue: integer);
begin
  if FLoadedSoundIndex = AValue then
    Exit;
  FLoadedSoundIndex := AValue;
end;

procedure TfraTimer.SetMetronome(AValue: boolean);
begin
  if FMetronome = AValue then
    Exit;
  FMetronome := AValue;
  if Running then
    if FMetronome then
      MetronomeInstance.Start
    else
      MetronomeInstance.Stop;
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

procedure TfraTimer.SetSoundIndex(AValue: integer);
begin
  FSoundIndex := AValue;
end;

procedure TfraTimer.SetStopButtonEnabled(AValue: boolean);
begin
  bbStop.Enabled := AValue;
end;

//function TfraTimer.GetId: longword;
//begin
//  Result := FId;
//end;

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

procedure TfraTimer.UpdateProgress(const PendingMilliseconds: QWord);
var
  ProgressPercentage: single;
  CounterText: string;
  Seconds: integer;
  Minutes: integer;
  Hours: integer;
  Elapsed: QWord;
begin
  Elapsed := Ceil(PendingMilliseconds / 1000);

  Seconds := Elapsed mod 60;
  Minutes := Elapsed div 60;
  Hours := Minutes div 60;
  Minutes := Minutes mod 60;
  CounterText := Format('%.2d', [Hours]) + ':' + Format('%.2d', [Minutes]) +
    ':' + Format('%.2d', [Seconds]);

  if Counter <> CounterText then
    Counter := CounterText;

  { Calculate percentage ProgressPercentage }

  Assert(FOrigTickDuration > 0);

  ProgressPercentage := 1 - (PendingMilliseconds / FOrigTickDuration);

  { Elapsed time can exceed total pending tick duration, in certain cases.
  The system could go on sleep mode while a timer is running
  (and while the timer event is being processed) and on
  waking up, the pre-caculated tick duration could have already been
  overshot. If that is the case, mark ProgressPercentage as zero, and the next
  timer event will mark it as completed. }
  if ProgressPercentage < 0 then
    ProgressPercentage := 0;
  Assert(ProgressPercentage <= 1);
  PUblishProgress(1 - (PendingMilliseconds / FOrigTickDuration));

end;

procedure TfraTimer.ReenableEditControls;
begin
  { When a timer stops, check if the same timer was being edited?
  If so, a few controls which were disabled can be re-enabled }

  if frmEdit.Showing and FBeingEdited then
  begin
    frmEdit.ReenableControls;
  end;
end;

//procedure TfraTimer.ArrangeControls;
//begin
{ Horizontally arrange controls }
  {cbSelect.Left := TIMER_PADDING;
  imgTimer.Left := cbSelect.Left + cbSelect.Width + TIMER_PADDING;
  edtTitle.Left := imgTimer.Left + imgTimer.Width + TIMER_PADDING;

  bbEdit.Left := ClientWidth - bbEdit.Width - TIMER_PADDING;
  ckbIconProgress.Left := bbEdit.Left - ckbIconProgress.Width - TIMER_REPORT_PADDING;
  lblCountdown.Left := ckbIconProgress.Left - lblCountdown.Width - TIMER_REPORT_PADDING;
  bbAdjust.Left := lblCountdown.Left - bbAdjust.Width - TIMER_PADDING;
  bbStop.Left := bbAdjust.Left - bbStop.Width - TIMER_PADDING;
  bbPause.Left := bbStop.Left - bbPause.Width - TIMER_PADDING;
  bbPlay.Left := bbPause.Left - bbPlay.Width - TIMER_PADDING;
  dtpSet.Left := bbPlay.Left - dtpSet.Width - TIMER_PADDING;
  edtTitle.Width := dtpSet.Left - edtTitle.Left - TIMER_PADDING;}

{ Vertically centre controls }
//cbSelect.Top := (Height - cbSelect.Height) div 2;
//imgTimer.Top := (Height - imgTimer.Height) div 2;
//edtTitle.Top := (Height - edtTitle.Height) div 2;
//dtpSet.Top := (Height - dtpSet.Height) div 2;
//bbPlay.Top := (Height - bbPlay.Height) div 2;
//bbPause.Top := (Height - bbPause.Height) div 2;
//bbStop.Top := (Height - bbStop.Height) div 2;
//bbAdjust.Top := (Height - bbAdjust.Height) div 2;
//lblCountdown.Top := (Height - lblCountdown.Height) div 2;
//ckbIconProgress.Top := (Height - ckbIconProgress.Height) div 2;
//bbEdit.Top := (Height - bbEdit.Height) div 2;

//end;

function TfraTimer.RestartFromLastFinish: boolean;
var
  CurrentTime: TDateTime;
  NewEndTickCount, Adjustment: QWord;
  //StartTickCount: QWord = 0;
begin
  Result := False;

  Assert(not Running);

  CurrentTime := Now;

  { Logging important variables to troubleshoot issues with restart from last
  stop. }

  Logger.Debug('FLastCompletionTime - ' + DateTimeToStr(FLastCompletionTime) +
    ' > ' + FloatToStr(FLastCompletionTime));
  Logger.Debug('FLastCompletionTime + Duration - ' +
    DateTimeToStr(FLastCompletionTime + Duration) + ' > ' +
    FloatToStr(FLastCompletionTime + Duration));
  Logger.Debug('CurrentTime - ' + DateTimeToStr(CurrentTime) + ' > ' +
    FloatToStr(CurrentTime));

  { Is it too late to re-start? }
  //if (FLastCompletionTime + Duration) <= CurrentTime then
  //begin
  //  Logger.Debug('Too late to re-start');
  //  Exit;
  //end;

  { Wait for audio player to finish before starting again. }
  AbortSound(True);

  { Calculate the adjustment prior to starting the timer. The process of
  starting the timer changes FLastCompletionTime }

  Logger.Debug('FLastCompletionTime (after Start) - ' +
    DateTimeToStr(FLastCompletionTime) + ' > ' + FloatToStr(FLastCompletionTime));

  Adjustment := MilliSecondsBetween(CurrentTime, FLastCompletionTime);

  Logger.Debug('Adjustment (MilliSecondsBetween(CurrentTime, FLastCompletionTime)) - ' +
    IntToStr(Adjustment));

  Start;

  Assert(FEndTickCount > Adjustment);

  NewEndTickCount := FEndTickCount - Adjustment;
  Logger.Debug('FEndTickCount - ' + IntToStr(FEndTickCount));
  Logger.Debug('NewEndTickCount - ' + IntToStr(NewEndTickCount));

  FEndTickCount := NewEndTickCount;
  HandleTimerTrigger();
  Result := True;
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

  ReenableEditControls;
end;

procedure TfraTimer.Hide;
begin
  Top := Top + Height;
end;

constructor TfraTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitCriticalSection(AudioCriticalSection);
  //FId := 0;
  FProgress := 0.0;

  with UserConfig do
  begin
    dtpSet.Time := DefaultTimerDuration;
    edtTitle.Text := DefaultTimerTitle;
  end;

  //Parent := TWinControl(AOwner);

  cbSelect.OnChange := @ClockSelected;

  FLoadedSoundIndex := INVALID_SOUNDPOOL_INDEX;
  FLoadedSoundSource := '';

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

  FAudioPlayer := nil;
  if AudioSystem.Loaded then
  begin
    FSoundIndex := SoundPool.DefaultSoundIndex;
    FAudioPlayer := TAudioPlayer.Create;
    FAudioPlayer.OnPlayCompletion := @AudioPlayed;
  end;

  Metronome := False;

  FLastCompletionTime := 0;

  FBeingAdjusted := False;
  FBeingEdited := False;
  //ArrangeControls;

  LayoutControls;

end;

destructor TfraTimer.Destroy;
begin
  FAudioPlayer.Free;
  Parent := nil;

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
  { If Hours, Minutes, Seconds, all are zero then disable play button. }
  bbPlay.Enabled := not ((Hours = 0) and (Minutes = 0) and (Seconds = 0));
end;

procedure TfraTimer.HandleTimerTrigger();
var
  PendingMilliseconds: QWord;
  CurrTickCount: QWord;
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

  AbortSound(True);

  if FPaused = False then
  begin
    FStartTickCount := GetTickCount64;

    FEndTickCount :=
      FStartTickCount + (((3600 * QWord(Hours)) + (60 * QWord(Minutes)) +
      QWord(Seconds)) * 1000);

    FOrigTickDuration := FEndTickCount - FStartTickCount;
    if UserConfig.AutoProgress = True then
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

  frmMain.TimerStarted(Self);

  if Metronome then
    MetronomeInstance.Start;

end;

procedure TfraTimer.Pause;
var
  CurrTickCount: QWord;
begin
  CurrTickCount := GetTickCount64;

  {Save the pending tick counts.
  While doing this if we see that there are no pending tick counts, finish
  instead of stop.}

  Assert(FEndTickCount > 0);
  if FEndTickCount <= CurrTickCount then
    Finish
  else
    FPendingTickCount := FEndTickCount - CurrTickCount;

  FPaused := True;

  PlayButtonEnabled := True;
  PauseButtonEnabled := False;
  StopButtonEnabled := True;
  DurationEnabled := False;

  frmMain.TimerPaused(Self);

  if Metronome then
    MetronomeInstance.Stop;

end;

procedure TfraTimer.Stop(UserInitiated: boolean);
var
  Device: TAudioDevice;
begin
  { The audio is playing and the user request is to terminate the audio. }
  Logger.Debug('Entering Stop. UserInitiated - ' +
    IfThen(UserInitiated, 'True', 'False'));

  if AudioSystem.Loaded and FAudioPlayer.Playing then
  begin
    FRunning := False;
    FPaused := False;
    PlayButtonEnabled := True;
    PauseButtonEnabled := False;
    StopButtonEnabled := False;
    DurationEnabled := True;

    bbAdjust.Enabled := False;

    {There is no need to close the stream. Stopping/aborting the stream
    will trigger the callback for stream stoppage. The stream will be closed
    in that callback function}
    FAudioPlayer.Abort;

    Exit;
  end
  else
    {The timer run has completed. Stop the timer and play audio if required}
  begin

    { If already stopped (and if audio not playing), then the timer is
    already stopped completely. }

    if not FRunning then
    begin
      Logger.Debug('Timer is already stopped.');
      Exit;
    end;

    FLastCompletionTime := Now;
    { Stop the Metronome if it is running. }
    if Metronome and not Paused then
      MetronomeInstance.Stop;

    FRunning := False;
    FPaused := False;
    Counter := DEF_COUNTDOWN_CAPTION;
    bbAdjust.Enabled := False;

    PublishProgress(TIMER_PROGRESS_FINISHED);

    FEndTickCount := 0;
    FOrigTickDuration := 0;
    FStartTickCount := 0;

    if frmAdjust.Showing and FBeingAdjusted then
      frmAdjust.bbApply.Enabled := False;

    PauseButtonEnabled := False;
    DurationEnabled := True;


    if AudioSystem.Loaded and (FSoundIndex >= SoundPool.DefaultSoundIndex) and
      (not UserInitiated) then
    begin
      PlayButtonEnabled := False;
      StopButtonEnabled := True;

      AudioSystem.UseDefaultDevice := UserConfig.UseDefaultAudioDevice;

      if not AudioSystem.UseDefaultDevice then
      begin
        if (UserConfig.AudioDeviceName = '') or
          (UserConfig.AudioHostAPIName = '') then
          AudioSystem.SetDefaulDevice
        else
        begin
          Device.DeviceName := UserConfig.AudioDeviceName;
          Device.HostAPIName := UserConfig.AudioHostAPIName;
          AudioSystem.OutputDevice := Device;
        end;

      end;
      ReenableEditControls;
      { Play the sound as per configuration. }
      if FSoundIndex >= SoundPool.DefaultSoundIndex then
      begin
        FAudioPlayer.AmplitudeScalePoller := @AudioSystem.GetAmplitudeScale;
        FAudioPlayer.Play(SoundPool.RawSound[FSoundIndex]);
      end;
    end
    else
    begin
      PlayButtonEnabled := True;
      StopButtonEnabled := False;

      ReenableEditControls;
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
  NewEndTickCount, CurrTickCount, Adjustment: QWord;
  Diff: int64;
  EndTime: TDateTime;
begin

  { If the timer is not running, there is nothing to do
  We should not even be in such a situation, but that is a different. }
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

      Adjustment := (((3600 * QWord(Hours)) + (60 * QWord(Mins)) +
        QWord(Secs)) * 1000);
      if FPaused = False then
      begin
        if Adjustment > FEndTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;
          { Adjustment is too large and cannot be used for computations
          As a circumvention, set NewEndTickCount to 0. }
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
      else  { If paused }
      begin
        if Adjustment > FPendingTickCount then
        begin
          if MessageDlg('Confirm', 'This will stop the timer',
            mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
            Exit;

          Stop(True);
        end
        else
        begin
          FPendingTickCount := FPendingTickCount - Adjustment;
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

      Adjustment := (((3600 * QWord(Hours)) + (60 * QWord(Mins)) +
        QWord(Secs)) * 1000);
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
      { No need for UTC, as this is base don local TZ time selected by user }
      EndTime := IncMilliSecond(Now, (FEndTickCount - CurrTickCount));

      { If extension }
      if EndTime < frmAdjust.dtpTill.DateTime then
      begin
        Diff := MilliSecondsBetween(EndTime, frmAdjust.dtpTill.DateTime);
        Inc(FEndTickCount, Diff);
        Inc(FOrigTickDuration, Diff);
      end
      else { If shortening }
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
    end
    else;
  end;
end;

procedure TfraTimer.SaveState(var SaveTo: TTimerState);
var
  CurrTickCount, DiffTicks: QWord;
begin
  with SaveTo do
  begin
    Running := FRunning;
    Paused := FPaused;
    PendingTicks := FPendingTickCount;

    { FPendingTickCount is reliable only if the timer is paused. }
    CurrTickCount := GetTickCount64;
    DiffTicks := 0;
    if FEndTickCount > CurrTickCount then
      DiffTicks := FEndTickCount - CurrTickCount;
    EndTime := IncMilliSecond(LocalTimeToUniversal(Now), DiffTicks);
    DurationTicks := FOrigTickDuration;
  end;
end;

procedure TfraTimer.LoadState(var LoadFrom: TTimerState);
var
  NewPendingTickCount: QWord;
  TimeNow: TDateTime;
begin
  { If any of the timers saved as running/paused re-instate the same state}
  if LoadFrom.Running and (not FRunning) then
  begin
    if LoadFrom.Paused then
    begin
      FPaused := True;

      PlayButtonEnabled := True;
      PauseButtonEnabled := False;
      StopButtonEnabled := True;
      DurationEnabled := False;

      with LoadFrom do
      begin
        FRunning := Running;
        FPaused := Paused;
        FPendingTickCount := PendingTicks;
        FOrigTickDuration := DurationTicks;
      end;
    end
    else
    begin
      { Now moves as the processer executes. Save it in a variable. }
      TimeNow := LocalTimeToUniversal(Now);
      if LoadFrom.EndTime <= TimeNow then
      begin

        frmMain.TimerFinished(Self, False);
      end
      else
      begin
        NewPendingTickCount := MilliSecondsBetween(TimeNow, LoadFrom.EndTime);
        Start;
        with LoadFrom do
        begin
          FRunning := Running;
          FPaused := Paused;
          FPendingTickCount := NewPendingTickCount;
          FEndTickCount := GetTickCount64 + NewPendingTickCount;
          FOrigTickDuration := DurationTicks;
        end;
      end;
    end;
  end;
end;

procedure TfraTimer.AbortSound(Wait: boolean);
var
  StartTickCount: QWord;
begin
  if not AudioSystem.Loaded then Exit;

  if not FAudioPlayer.Playing then Exit;

  FAudioPlayer.Abort;
  if Wait then
  begin
    StartTickCount := GetTickCount64;
    { Wait for FAudio to complete. }
    if FAudioPlayer.Playing then
    begin
      FAudioPlayer.Abort;
      while FAudioPlayer.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
        begin
          Logger.Warning('FAudioPlayer Abort did not complete. ' +
            'Cannot restart timer');
          Exit;
        end;
      end;
    end;
  end;
end;

function TfraTimer.QueryRestartFromLastFinish: boolean;
begin
  Assert(not Running);

  { Is it too late to re-start? }
  Result := ((FLastCompletionTime + Duration) > Now);

end;


end.
