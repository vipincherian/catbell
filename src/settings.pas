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
unit settings;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, Forms, {Dialogs, }{Graphics,} DateTimePicker, jsonConf,
  audio, {EventLog,} LCLIntf, LCLType, constants, Math;

type
  { TUserConfig }

  TUserConfig = class(TObject)
  private
    FDefaultPos: TRect;
    FLatency: integer;
    function GetAppIconSize: integer;
    function GetDefaultStartPosition: TRect;
    function GetTrayIconSize: integer;
    procedure SetLatency(AValue: integer);
  public
    ShowModalAlert: boolean;
    ShowTrayAlert: boolean;
    AutoProgress: boolean;
    LastPosNormal: TRect;
    LastPosRestored: TRect;
    QueryExit: boolean;
    AllowTimerTitleEdit: boolean;
    DefaultTimerTitle: string;

    DefaultTimerDuration: double;

    LastWindowState: TWindowState;
    DefaultTimeFormat: integer;

    AdjustDiff: double;
    AdjustCompleteby: double;

    UseDefaultAudioDevice: boolean;
    AudioDeviceName: string;
    AudioHostAPIName: string;

    TaskbarIconType: TTaskbarIconType;

    UseDefaultSound: boolean;
    LoopSound: boolean;

    Volume: integer;

    //Bpm: integer;

    TrayIconSizeOverridden: integer;
    AppIconSizeOverridden: integer;
    DefaultTrayIconSize: integer;
    DefaultAppIconSize: integer;

    OverrideTrayIconSize: boolean;
    OverrideAppIconSize: boolean;

    RestartFromFinish: boolean;

    OverrideLatency: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CopyFrom(From: TUserConfig);
    function CompareWith(From: TUserConfig): boolean;

    property TrayIconSize: integer read GetTrayIconSize;
    property AppIconSize: integer read GetAppIconSize;
    property Latency: integer read FLatency write SetLatency;


  end;

  { TUserFileConfig }

  TUserFileConfig = class(TUserConfig)
  private
    FFileName: string;
    FConf: TJSONConfig;
    procedure Save;
    procedure CreateAnew;

  public
    constructor Create(FileName: string);
    destructor Destroy; override;
    procedure Load;
    procedure Flush;
  end;

var
  //GlobalDefault: TDefaultConfig;
  UserConfig: TUserFileConfig = nil;
//Logger: TEventLog;


//procedure InitSettings;
//procedure CleanupSettings;

implementation

//procedure InitSettings;
//begin
//  { Combo-box indices are set blindly. An assert to check this during development. }
//  Assert(integer(tf12) = 0);
//  Assert(integer(tf24) = 1);

//  if not DirectoryExists(GetAppConfigDir(False)) then
//    CreateDir(GetAppConfigDir(False));

//  UserConfig := TUserFileConfig.Create(GetAppConfigDir(False) + 'user.json');

//  UserConfig.Load;
//end;

//procedure CleanupSettings;
//begin
//  UserConfig.Free;
//end;



{ TUserFileConfig }

constructor TUserFileConfig.Create(FileName: string);
begin
  { Constructor cannot be hidden unless it is made strict private.
  Constructor will proceed with creation only if the single instance is not
  created yet. Once created, the same instance is returned.}
  if not Assigned(UserConfig) then
  begin
    inherited Create;
    FFileName := FileName;
    FConf := TJSONConfig.Create(nil);
    FConf.Filename := FFileName;
    FConf.Formatted := True;
  end
  else
    Self := UserConfig;
end;

destructor TUserFileConfig.Destroy;
begin
  Save;
  FConf.Free;
  inherited Destroy;
end;

procedure TUserFileConfig.Load;
begin

  FConf.Filename := FFileName;
  if not FileExists(FFileName) then
  begin
    CreateAnew;
  end;
  try
    FConf.OpenKey(LAST_POS_NORMAL, False);
    LastPosNormal.Top := FConf.GetValue(LAST_POS_TOP, FDefaultPos.Top);
    LastPosNormal.Left := FConf.GetValue(LAST_POS_LEFT, FDefaultPos.Left);
    LastPosNormal.Right := FConf.GetValue(LAST_POS_RIGHT, FDefaultPos.Right);
    LastPosNormal.Bottom := FConf.GetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
    FConf.CloseKey;

    FConf.OpenKey(LAST_POS_RESTORED, False);
    LastPosRestored.Top := FConf.GetValue(LAST_POS_TOP, FDefaultPos.Top);
    LastPosRestored.Left := FConf.GetValue(LAST_POS_LEFT, FDefaultPos.Left);
    LastPosRestored.Right := FConf.GetValue(LAST_POS_RIGHT, FDefaultPos.Right);
    LastPosRestored.Bottom := FConf.GetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
    FConf.CloseKey;

    LastWindowState := TWindowState(FConf.GetValue(WINDOW_STATE, DEF_WINDOW_STATE));

    ShowModalAlert := FConf.GetValue(SHOW_MODAL_ALERT, DEF_SHOW_MODAL_ALERT);
    ShowTrayAlert := FConf.GetValue(SHOW_TRAY_ALERT, DEF_SHOW_TRAY_ALERT);

    AutoProgress := FConf.GetValue(AUTO_PROGRESS, AutoProgress);
    QueryExit := FConf.GetValue(QUERY_EXIT, QueryExit);
    AllowTimerTitleEdit := FConf.GetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
    DefaultTimerTitle := string(FConf.GetValue(UTF8Decode(TIMER_TITLE),
      UTF8Decode(DefaultTimerTitle)));

    DefaultTimerDuration := FConf.GetValue(TIMER_DURATION, DefaultTimerDuration);

    DefaultTimeFormat := FConf.GetValue(TIME_FORMAT, DefaultTimeFormat);

    AdjustDiff := FConf.GetValue(ADJ_DIFF, DEF_ADJ_DIFF);
    if AdjustDiff <= 0 then
      AdjustDiff := DEF_ADJ_DIFF;

    AdjustCompleteby := FConf.GetValue(ADJ_COMPLETEBY, DEF_ADJ_COMPLETEBY);
    if AdjustCompleteby <= 0 then
      AdjustCompleteby := DEF_ADJ_COMPLETEBY;

    AudioDeviceName := string(FConf.GetValue(AUDIO_DEVICE_NAME, AudioDeviceName));
    AudioHostAPIName := string(FConf.GetValue(AUDIO_HOSTAPI_NAME,
      AudioHostAPIName));
    UseDefaultAudioDevice :=
      FConf.GetValue(USE_DEFAULT_AUDIO_DEVICE, UseDefaultAudioDevice);

    TaskbarIconType := TTaskbarIconType(FConf.GetValue(TASKBAR_ICON_TYPE,
      integer(DEF_TASKBAR_ICON_TYPE)));

    UseDefaultAudioDevice := FConf.GetValue(USE_DEFAULT_SOUND,
      DEF_USE_DEFAULT_AUDIO_DEVICE);
    LoopSound := FConf.GetValue(LOOP_SOUND, DEF_LOOP_SOUND);
    Volume := FConf.GetValue(VOLUME_LEVEL, DEF_VOLUME_LEVEL);
    //Bpm := FConf.GetValue(METRONOME_BPM, DEF_METRONOME_BPM);

    TrayIconSizeOverridden := FConf.GetValue(TRAY_ICON_SIZE, DefaultTrayIconSize);
    AppIconSizeOverridden := FConf.GetValue(APP_ICON_SIZE, DefaultAppIconSize);

    OverrideTrayIconSize := FConf.GetValue(OVERRIDE_TRAY_ICON_SIZE,
      DEF_OVERRIDE_TRAY_ICON_SIZE);
    OverrideAppIconSize := FConf.GetValue(OVERRIDE_APP_ICON_SIZE,
      DEF_OVERRIDE_APP_ICON_SIZE);

    RestartFromFinish := FConf.GetValue(RESTART_FROM_FINISH, DEF_RESTART_FROM_FINISH);

    OverrideLatency := FConf.GetValue(OVERRIDE_LATENCY, DEF_OVERRIDE_LATENCY);
    Latency := FConf.GetValue(AUDIO_LATENCY, DEF_AUDIO_LATENCY);

  except
    CreateAnew;
  end;

end;

procedure TUserFileConfig.Save;
begin

  FConf.OpenKey(LAST_POS_NORMAL, True);
  FConf.SetValue(LAST_POS_TOP, LastPosNormal.Top);
  FConf.SetValue(LAST_POS_LEFT, LastPosNormal.Left);
  FConf.SetValue(LAST_POS_RIGHT, LastPosNormal.Right);
  FConf.SetValue(LAST_POS_BOTTOM, LastPosNormal.Bottom);
  FConf.CloseKey;

  FConf.OpenKey(LAST_POS_RESTORED, True);
  FConf.SetValue(LAST_POS_TOP, LastPosRestored.Top);
  FConf.SetValue(LAST_POS_LEFT, LastPosRestored.Left);
  FConf.SetValue(LAST_POS_RIGHT, LastPosRestored.Right);
  FConf.SetValue(LAST_POS_BOTTOM, LastPosRestored.Bottom);
  Fconf.CloseKey;

  FConf.SetValue(WINDOW_STATE, integer(LastWindowState));

  FConf.SetValue(SHOW_MODAL_ALERT, ShowModalAlert);
  FConf.SetValue(SHOW_TRAY_ALERT, ShowTrayAlert);
  FConf.SetValue(AUTO_PROGRESS, AutoProgress);
  FConf.SetValue(QUERY_EXIT, QueryExit);
  FConf.SetValue(ALLOW_TIMERTITLE_EDIT, AllowTimerTitleEdit);
  Fconf.SetValue(TIMER_TITLE, DefaultTimerTitle);

  FConf.SetValue(TIMER_DURATION, DefaultTimerDuration);

  FConf.SetValue(TIME_FORMAT, DefaultTimeFormat);

  FConf.SetValue(ADJ_DIFF, AdjustDiff);
  FConf.SetValue(ADJ_COMPLETEBY, AdjustCompleteby);

  FConf.SetValue(AUDIO_DEVICE_NAME, AudioDeviceName);
  FConf.SetValue(AUDIO_HOSTAPI_NAME, AudioHostAPIName);
  FConf.SetValue(USE_DEFAULT_AUDIO_DEVICE, UseDefaultAudioDevice);

  FConf.SetValue(TASKBAR_ICON_TYPE, integer(TaskbarIconType));

  FConf.SetValue(USE_DEFAULT_SOUND, UseDefaultSound);
  FConf.SetValue(LOOP_SOUND, LoopSound);
  FConf.SetValue(VOLUME_LEVEL, Volume);
  //Fconf.SetValue(METRONOME_BPM, Bpm);

  FConf.SetValue(TRAY_ICON_SIZE, TrayIconSizeOverridden);
  FConf.SetValue(APP_ICON_SIZE, AppIconSizeOverridden);

  FConf.SetValue(OVERRIDE_TRAY_ICON_SIZE, OverrideTrayIconSize);
  FConf.SetValue(OVERRIDE_APP_ICON_SIZE, OverrideAppIconSize);

  FConf.SetValue(RESTART_FROM_FINISH, RestartFromFinish);

  FConf.SetValue(OVERRIDE_LATENCY, OverrideLatency);
  FConf.SetValue(AUDIO_LATENCY, Latency);

end;

procedure TUserFileConfig.CreateAnew;
var
  AudioDevice: TAudioDevice;
begin
  FConf.OpenKey(LAST_POS_NORMAL, True);
  FConf.SetValue(LAST_POS_TOP, FDefaultPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FDefaultPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FDefaultPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
  FConf.CloseKey;

  FConf.OpenKey(LAST_POS_RESTORED, True);
  FConf.SetValue(LAST_POS_TOP, FDefaultPos.Top);
  FConf.SetValue(LAST_POS_LEFT, FDefaultPos.Left);
  FConf.SetValue(LAST_POS_RIGHT, FDefaultPos.Right);
  FConf.SetValue(LAST_POS_BOTTOM, FDefaultPos.Bottom);
  Fconf.CloseKey;

  FConf.SetValue(WINDOW_STATE, DEF_WINDOW_STATE);

  FConf.SetValue(SHOW_MODAL_ALERT, DEF_SHOW_MODAL_ALERT);
  FConf.SetValue(SHOW_TRAY_ALERT, DEF_SHOW_TRAY_ALERT);
  FConf.SetValue(AUTO_PROGRESS, DEF_AUTO_PROGRESS);
  FConf.SetValue(QUERY_EXIT, DEF_QUERY_EXIT);
  Fconf.SetValue(TIMER_TITLE, DEF_TIMER_TITLE);
  FConf.SetValue(TIMER_HOURS, DEF_TIMER_HOURS);
  FConf.SetValue(TIMER_MINS, DEF_TIMER_MINS);
  FConf.SetValue(TIMER_SECS, DEF_TIMER_SECS);

  FConf.SetValue(TIME_FORMAT, integer(DEF_TIME_FORMAT));

  FConf.SetValue(ADJ_DIFF, DEF_TIMER_DURATION);
  FConf.SetValue(ADJ_COMPLETEBY, DEF_TIMER_DURATION);

  if AudioSystem.Loaded then
  begin
    AudioSystem.GetDefaultDevice(@AudioDevice);
    FConf.SetValue(AUDIO_DEVICE_NAME, AudioDevice.DeviceName);
    FConf.SetValue(AUDIO_HOSTAPI_NAME, AudioDevice.HostAPIName);
    FConf.SetValue(USE_DEFAULT_AUDIO_DEVICE, DEF_USE_DEFAULT_AUDIO_DEVICE);
  end
  else
  begin
    FConf.SetValue(AUDIO_DEVICE_NAME, DEF_AUDIO_DEVICE_NAME);
    FConf.SetValue(AUDIO_HOSTAPI_NAME, DEF_AUDIO_HOSTAPI_NAME);
    FConf.SetValue(USE_DEFAULT_AUDIO_DEVICE, DEF_USE_DEFAULT_AUDIO_DEVICE);
  end;

  FConf.SetValue(TASKBAR_ICON_TYPE, integer(DEF_TASKBAR_ICON_TYPE));

  FConf.SetValue(USE_DEFAULT_SOUND, UseDefaultSound);
  FConf.SetValue(LOOP_SOUND, LoopSound);
  FConf.SetValue(VOLUME_LEVEL, Volume);
  //FConf.SetValue(METRONOME_BPM, Bpm);

  FConf.SetValue(TRAY_ICON_SIZE, TrayIconSizeOverridden);
  FConf.SetValue(APP_ICON_SIZE, AppIconSizeOverridden);

  FConf.SetValue(OVERRIDE_TRAY_ICON_SIZE, OverrideTrayIconSize);
  FConf.SetValue(OVERRIDE_APP_ICON_SIZE, OverrideAppIconSize);

  FConf.SetValue(RESTART_FROM_FINISH, DEF_RESTART_FROM_FINISH);

  FConf.SetValue(OVERRIDE_LATENCY, DEF_OVERRIDE_LATENCY);
  FConf.SetValue(AUDIO_LATENCY, DEF_AUDIO_LATENCY);

  FConf.Flush;
end;

procedure TUserFileConfig.Flush;
begin
  FConf.Flush;
end;


{ TUserConfig }



function TUserConfig.GetDefaultStartPosition: TRect;
var
  Default: TRect;
begin

  Default.Left := Screen.Width div 6;
  Default.Top := Screen.Height div 6;

  Default.Right := Default.Left + ((2 * Screen.Width) div 3);
  Default.Bottom := Default.Top + ((2 * Screen.Height) div 3);

  Result := Default;

end;

function TUserConfig.GetAppIconSize: integer;
begin
  if OverrideAppIconSize then
    Result := AppIconSizeOverridden
  else
    Result := DefaultAppIconSize;
end;

function TUserConfig.GetTrayIconSize: integer;
begin
  if OverrideTrayIconSize then
    Result := TrayIconSizeOverridden
  else
    Result := DefaultTrayIconSize;
end;

procedure TUserConfig.SetLatency(AValue: integer);
begin
  if FLatency = AValue then
    Exit;
  FLatency := Max(AUDIO_LATENCY_MIN, Min(AValue, AUDIO_LATENCY_MAX));
end;


constructor TUserConfig.Create;
begin

  FDefaultPos := GetDefaultStartPosition;

  ShowModalAlert := DEF_SHOW_MODAL_ALERT;
  ShowTrayAlert := DEF_SHOW_TRAY_ALERT;
  AutoProgress := DEF_AUTO_PROGRESS;
  QueryExit := DEF_QUERY_EXIT;

  LastWindowState := wsNormal;
  DefaultTimerTitle := DEF_TIMER_TITLE;

  DefaultTimerDuration := DEF_TIMER_DURATION;

  DefaultTimeFormat := integer(DEF_TIME_FORMAT);

  AudioDeviceName := DEF_AUDIO_DEVICE_NAME;
  AudioHostAPIName := DEF_AUDIO_HOSTAPI_NAME;
  UseDefaultAudioDevice := DEF_USE_DEFAULT_AUDIO_DEVICE;

  TaskbarIconType := DEF_TASKBAR_ICON_TYPE;

  UseDefaultSound := DEF_USE_DEFAULT_SOUND;
  LoopSound := DEF_LOOP_SOUND;
  Volume := DEF_VOLUME_LEVEL;
  //Bpm := DEF_METRONOME_BPM;

  AdjustCompleteby := DEF_ADJ_COMPLETEBY;
  AdjustDiff := DEF_ADJ_DIFF;

  DefaultTrayIconSize := GetSystemMetrics(SM_CXSMICON);
  DefaultAppIconSize := GetSystemMetrics(SM_CXICON);

  TrayIconSizeOverridden := DefaultTrayIconSize;
  AppIconSizeOverridden := DefaultAppIconSize;

  OverrideTrayIconSize := DEF_OVERRIDE_TRAY_ICON_SIZE;
  OverrideAppIconSize := DEF_OVERRIDE_APP_ICON_SIZE;

  RestartFromFinish := DEF_RESTART_FROM_FINISH;

  OverrideLatency := DEF_OVERRIDE_LATENCY;
  Latency := DEF_AUDIO_LATENCY;
end;

destructor TUserConfig.Destroy;
begin
  inherited Destroy;
end;

procedure TUserConfig.CopyFrom(From: TUserConfig);
begin
  ShowModalAlert := From.ShowModalAlert;
  ShowTrayAlert := From.ShowTrayAlert;
  AutoProgress := From.AutoProgress;
  QueryExit := From.QueryExit;

  DefaultTimerTitle := From.DefaultTimerTitle;

  DefaultTimerDuration := From.DefaultTimerDuration;
  AllowTimerTitleEdit := From.AllowTimerTitleEdit;
  DefaultTimeFormat := From.DefaultTimeFormat;

  AdjustDiff := From.AdjustDiff;
  AdjustCompleteby := From.AdjustCompleteby;

  AudioDeviceName := From.AudioDeviceName;
  AudioHostAPIName := From.AudioHostAPIName;
  UseDefaultAudioDevice := From.UseDefaultAudioDevice;

  TaskbarIconType := From.TaskbarIconType;

  UseDefaultSound := From.UseDefaultSound;
  LoopSound := From.LoopSound;
  Volume := From.Volume;
  //Bpm := From.Bpm;

  TrayIconSizeOverridden := From.TrayIconSizeOverridden;
  AppIconSizeOverridden := From.AppIconSizeOverridden;

  OverrideTrayIconSize := From.OverrideTrayIconSize;
  OverrideAppIconSize := From.OverrideAppIconSize;

  RestartFromFinish := From.RestartFromFinish;

  OverrideLatency := From.OverrideLatency;
  Latency := From.Latency;

end;

function TUserConfig.CompareWith(From: TUserConfig): boolean;
begin

  Result := False;

  if ShowModalAlert <> From.ShowModalAlert then
    Exit;

  if ShowTrayAlert <> From.ShowTrayAlert then
    Exit;

  if AutoProgress <> From.AutoProgress then
    Exit;

  if QueryExit <> From.QueryExit then
    Exit;

  if DefaultTimerTitle <> From.DefaultTimerTitle then
    Exit;

  if DefaultTimerDuration <> From.DefaultTimerDuration then
    Exit;

  if DefaultTimeFormat <> From.DefaultTimeFormat then
    Exit;

  if AdjustDiff <> From.AdjustDiff then
    Exit;

  if AdjustCompleteby <> From.AdjustCompleteby then
    Exit;

  if AudioDeviceName <> From.AudioDeviceName then
    Exit;

  if AudioHostAPIName <> From.AudioHostAPIName then
    Exit;

  if UseDefaultAudioDevice <> From.UseDefaultAudioDevice then
    Exit;

  if TaskbarIconType <> From.TaskbarIconType then
    Exit;

  if UseDefaultSound <> From.UseDefaultSound then
    Exit;

  if LoopSound <> From.LoopSound then
    Exit;

  if Volume <> From.Volume then
    Exit;

  //if Bpm <> From.Bpm then
  //begin
  //  Result := False;
  //  Exit;
  //end;

  if TrayIconSizeOverridden <> From.TrayIconSizeOverridden then
    Exit;

  if AppIconSizeOverridden <> From.AppIconSizeOverridden then
    Exit;

  if OverrideTrayIconSize <> From.OverrideTrayIconSize then
    Exit;

  if OverrideAppIconSize <> From.OverrideAppIconSize then
    Exit;

  // RestartFromFinish is not compared as it is not set in options form

  if OverrideLatency <> From.OverrideLatency then
    Exit;

  if Latency <> From.Latency then
    Exit;

  Result := True;
end;

initialization;

  { TODO : Fix this }
  { Combo-box indices are set blindly. An assert to check this during development. }
  Assert(integer(tf12) = 0);
  Assert(integer(tf24) = 1);

  if not DirectoryExists(GetAppConfigDir(False)) then
    CreateDir(GetAppConfigDir(False));

  UserConfig := TUserFileConfig.Create(GetAppConfigDir(False) + 'user.json');

  UserConfig.Load;

finalization;
  FreeAndNil(UserConfig);
end.
