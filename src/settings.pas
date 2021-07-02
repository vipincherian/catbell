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
unit settings;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Graphics, DateTimePicker, jsonConf,
  audio, {EventLog,} LCLIntf, LCLType;

type

  TTaskbarIconType = (TaskbarAppIcon, TaskbarOverlayIcon);
  { TUserConfig }

  TUserConfig = class(TObject)
  private
    FDefaultPos: TRect;
    function GetAppIconSize: integer;
    function GetDefaultStartPosition: TRect;
    function GetTrayIconSize: integer;
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

    Bpm: integer;

    TrayIconSizeOverridden: integer;
    AppIconSizeOverridden: integer;
    DefaultTrayIconSize: integer;
    DefaultAppIconSize: integer;

    OverrideTrayIconSize: boolean;
    OverrideAppIconSize: boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CopyFrom(From: TUserConfig);
    function CompareWith(From: TUserConfig): boolean;

    property TrayIconSize: integer read GetTrayIconSize;
    property AppIconSize: integer read GetAppIconSize;


  end;

  { TUserFileConfig }

  TUserFileConfig = class(TUserConfig)
  private
    FFileName: string;
    FConf: TJSONConfig;
    procedure Save;
    procedure CreateAnew;
    {%H-}constructor Create(FileName: string);
    {%H-}destructor {%H-}Destroy; override;
  public

    procedure Load;
    procedure Flush;
  end;

var
  //GlobalDefault: TDefaultConfig;
  UserConfig: TUserFileConfig;
//Logger: TEventLog;

const
  AUDIO_ABORT_SHORT_WAIT = 2000;
  AUDIO_ABORT_LONG_WAIT = 5000;

  TIMER_INIT_MINS = 'timer_init_mins';
  DEF_TIMER_INIT_MINS = 0;

  TIMER_INIT_SECS = 'timer_init_secs';
  DEF_TIMER_INIT_SECS = 10;

  TIMER_INIT_TITLE = 'timer_init_title';
  DEF_TIMER_INIT_TITLE = 'Countdown timer';

  LAST_POS_TOP = 'x';
  LAST_POS_LEFT = 'y';
  LAST_POS_BOTTOM = 'bottom';
  LAST_POS_RIGHT = 'right';

  LAST_POS_NORMAL = 'last_pos/normal/';
  LAST_POS_RESTORED = 'last_pos/restored/';


  SHOW_MODAL_ALERT = 'show_modal_alert';
  SHOW_TRAY_ALERT = 'show_tray_alert';
  DEF_SHOW_MODAL_ALERT = False;
  DEF_SHOW_TRAY_ALERT = True;

  AUTO_PROGRESS = 'auto_progress';
  DEF_AUTO_PROGRESS = True;

  QUERY_EXIT = 'query_exit';
  DEF_QUERY_EXIT = True;

  ALLOW_TIMERTITLE_EDIT = 'allow_timertitle_edit';
  DEF_ALLOW_TIMERTITLE_EDIT = False;

  TIMER_TITLE = 'timer_title';
  DEF_TIMER_TITLE = 'Countdown timer';

  TIMER_HOURS = 'timer_hours';
  DEF_TIMER_HOURS = 0;

  TIMER_MINS = 'timer_mins';
  DEF_TIMER_MINS = 10;

  TIMER_SECS = 'timer_secs';
  DEF_TIMER_SECS = 0;

  TIMER_DURATION = 'timer_duration';
  DEF_TIMER_DURATION = 3.4722222189884633E-003;

  WINDOW_STATE = 'window_state';
  DEF_WINDOW_STATE = integer(wsNormal);

  TIME_FORMAT = 'time_format';
  DEF_TIME_FORMAT = tf12;

  ADJ_DIFF = 'adjust_diff';
  DEF_ADJ_DIFF = DEF_TIMER_DURATION;
  ADJ_COMPLETEBY = 'adust_completeby';
  DEF_ADJ_COMPLETEBY = DEF_TIMER_DURATION;

  USE_DEFAULT_AUDIO_DEVICE = 'audio_device_default';
  DEF_USE_DEFAULT_AUDIO_DEVICE = True;
  AUDIO_HOSTAPI_NAME = 'audio_hostapi_name';
  DEF_AUDIO_HOSTAPI_NAME = '';
  AUDIO_DEVICE_NAME = 'audio_device_name';
  DEF_AUDIO_DEVICE_NAME = '';

  //TASKBAR_SHOW_APPICON = 1;
  //TASKBAR_SHOW_OVERLAY = 2;

  TASKBAR_ICON_TYPE = 'taskbar_appicon';
  {$IF defined(windows)}
  DEF_TASKBAR_ICON_TYPE = TaskbarOverlayIcon;
  {$ELSE}
  DEF_TASKBAR_ICON_TYPE = TaskbarAppIcon;
  {$ENDIF}

  USE_DEFAULT_SOUND = 'use_default_sound';
  DEF_USE_DEFAULT_SOUND = True;

  LOOP_SOUND = 'loop_sound';
  DEF_LOOP_SOUND = False;

  VOLUME_LEVEL = 'volume';
  DEF_VOLUME_LEVEL = DEFAULT_VOLUME;

  METRONOME_BPM = 'metronome_bpm';
  DEF_METRONOME_BPM = 100;

  TRAY_ICON_SIZE = 'tray_icon_size';
  APP_ICON_SIZE = 'app_icon_size';

  OVERRIDE_TRAY_ICON_SIZE = 'override_tray_icon_size';
  DEF_OVERRIDE_TRAY_ICON_SIZE = False;
  OVERRIDE_APP_ICON_SIZE = 'override_app_icon_size';
  DEF_OVERRIDE_APP_ICON_SIZE = False;



//procedure InitSettings;
//procedure CleanupSettings;

implementation

//procedure InitSettings;
//begin
//  { Combo-box indices are set blindly. An assert to check this during development. }
//  Assert(integer(tf12) = 0);
//  Assert(integer(tf24) = 1);
//
//  if not DirectoryExists(GetAppConfigDir(False)) then
//    CreateDir(GetAppConfigDir(False));
//
//  UserConfig := TUserFileConfig.Create(GetAppConfigDir(False) + 'user.json');
//
//  UserConfig.Load;
//end;
//
//procedure CleanupSettings;
//begin
//  UserConfig.Free;
//end;



{ TUserFileConfig }

constructor TUserFileConfig.Create(FileName: string);
begin
  inherited Create;
  FFileName := FileName;
  FConf := TJSONConfig.Create(nil);
  FConf.Filename := FFileName;
  FConf.Formatted := True;
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

    AudioDeviceName := string(FConf.GetValue(AUDIO_DEVICE_NAME,
      UTF8Decode(AudioDeviceName)));
    AudioHostAPIName := string(FConf.GetValue(AUDIO_HOSTAPI_NAME,
      UTF8Decode(AudioHostAPIName)));
    UseDefaultAudioDevice :=
      FConf.GetValue(USE_DEFAULT_AUDIO_DEVICE, UseDefaultAudioDevice);

    TaskbarIconType := TTaskbarIconType(FConf.GetValue(TASKBAR_ICON_TYPE,
      integer(DEF_TASKBAR_ICON_TYPE)));

    UseDefaultAudioDevice := FConf.GetValue(USE_DEFAULT_SOUND,
      DEF_USE_DEFAULT_AUDIO_DEVICE);
    LoopSound := FConf.GetValue(LOOP_SOUND, DEF_LOOP_SOUND);
    Volume := FConf.GetValue(VOLUME_LEVEL, DEF_VOLUME_LEVEL);
    Bpm := FConf.GetValue(METRONOME_BPM, DEF_METRONOME_BPM);

    TrayIconSizeOverridden := FConf.GetValue(TRAY_ICON_SIZE, DefaultTrayIconSize);
    AppIconSizeOverridden := FConf.GetValue(APP_ICON_SIZE, DefaultAppIconSize);

    OverrideTrayIconSize := FConf.GetValue(OVERRIDE_TRAY_ICON_SIZE,
      DEF_OVERRIDE_TRAY_ICON_SIZE);
    OverrideAppIconSize := FConf.GetValue(OVERRIDE_APP_ICON_SIZE,
      DEF_OVERRIDE_APP_ICON_SIZE);

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
  Fconf.SetValue(TIMER_TITLE, UTF8Decode(DefaultTimerTitle));

  FConf.SetValue(TIMER_DURATION, DefaultTimerDuration);

  FConf.SetValue(TIME_FORMAT, DefaultTimeFormat);

  FConf.SetValue(ADJ_DIFF, AdjustDiff);
  FConf.SetValue(ADJ_COMPLETEBY, AdjustCompleteby);

  FConf.SetValue(AUDIO_DEVICE_NAME, UTF8Decode(AudioDeviceName));
  FConf.SetValue(AUDIO_HOSTAPI_NAME, UTF8Decode(AudioHostAPIName));
  FConf.SetValue(USE_DEFAULT_AUDIO_DEVICE, UseDefaultAudioDevice);

  FConf.SetValue(TASKBAR_ICON_TYPE, integer(TaskbarIconType));

  FConf.SetValue(USE_DEFAULT_SOUND, UseDefaultSound);
  FConf.SetValue(LOOP_SOUND, LoopSound);
  FConf.SetValue(VOLUME_LEVEL, Volume);
  Fconf.SetValue(METRONOME_BPM, Bpm);

  FConf.SetValue(TRAY_ICON_SIZE, TrayIconSizeOverridden);
  FConf.SetValue(APP_ICON_SIZE, AppIconSizeOverridden);

  FConf.SetValue(OVERRIDE_TRAY_ICON_SIZE, OverrideTrayIconSize);
  FConf.SetValue(OVERRIDE_APP_ICON_SIZE, OverrideAppIconSize);

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
    FConf.SetValue(AUDIO_DEVICE_NAME, UTF8Decode(AudioDevice.DeviceName));
    FConf.SetValue(AUDIO_HOSTAPI_NAME, UTF8Decode(AudioDevice.HostAPIName));
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
  FConf.SetValue(METRONOME_BPM, Bpm);

  FConf.SetValue(TRAY_ICON_SIZE, TrayIconSizeOverridden);
  FConf.SetValue(APP_ICON_SIZE, AppIconSizeOverridden);

  FConf.SetValue(OVERRIDE_TRAY_ICON_SIZE, OverrideTrayIconSize);
  FConf.SetValue(OVERRIDE_APP_ICON_SIZE, OverrideAppIconSize);

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
  Bpm := DEF_METRONOME_BPM;

  AdjustCompleteby := DEF_ADJ_COMPLETEBY;
  AdjustDiff := DEF_ADJ_DIFF;

  DefaultTrayIconSize := GetSystemMetrics(SM_CXSMICON);
  DefaultAppIconSize := GetSystemMetrics(SM_CXICON);

  TrayIconSizeOverridden := DefaultTrayIconSize;
  AppIconSizeOverridden := DefaultAppIconSize;

  OverrideTrayIconSize := DEF_OVERRIDE_TRAY_ICON_SIZE;
  OverrideAppIconSize := DEF_OVERRIDE_APP_ICON_SIZE;
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
  Bpm := From.Bpm;

  TrayIconSizeOverridden := From.TrayIconSizeOverridden;
  AppIconSizeOverridden := From.AppIconSizeOverridden;

  OverrideTrayIconSize := From.OverrideTrayIconSize;
  OverrideAppIconSize := From.OverrideAppIconSize;

end;

function TUserConfig.CompareWith(From: TUserConfig): boolean;
begin
  if ShowModalAlert <> From.ShowModalAlert then
  begin
    Result := False;
    Exit;
  end;
  if ShowTrayAlert <> From.ShowTrayAlert then
  begin
    Result := False;
    Exit;
  end;
  if AutoProgress <> From.AutoProgress then
  begin
    Result := False;
    Exit;
  end;
  if QueryExit <> From.QueryExit then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimerTitle <> From.DefaultTimerTitle then
  begin
    Result := False;
    Exit;
  end;

  if DefaultTimerDuration <> From.DefaultTimerDuration then
  begin
    Result := False;
    Exit;
  end;
  if DefaultTimeFormat <> From.DefaultTimeFormat then
  begin
    Result := False;
    Exit;
  end;
  if AdjustDiff <> From.AdjustDiff then
  begin
    Result := False;
    Exit;
  end;
  if AdjustCompleteby <> From.AdjustCompleteby then
  begin
    Result := False;
    Exit;
  end;
  if AudioDeviceName <> From.AudioDeviceName then
  begin
    Result := False;
    Exit;
  end;
  if AudioHostAPIName <> From.AudioHostAPIName then
  begin
    Result := False;
    Exit;
  end;
  if UseDefaultAudioDevice <> From.UseDefaultAudioDevice then
  begin
    Result := False;
    Exit;
  end;
  if TaskbarIconType <> From.TaskbarIconType then
  begin
    Result := False;
    Exit;
  end;
  if UseDefaultSound <> From.UseDefaultSound then
  begin
    Result := False;
    Exit;
  end;
  if LoopSound <> From.LoopSound then
  begin
    Result := False;
    Exit;
  end;
  if Volume <> From.Volume then
  begin
    Result := False;
    Exit;
  end;
  if Bpm <> From.Bpm then
  begin
    Result := False;
    Exit;
  end;
  if TrayIconSizeOverridden <> From.TrayIconSizeOverridden then
  begin
    Result := False;
    Exit;
  end;
  if AppIconSizeOverridden <> From.AppIconSizeOverridden then
  begin
    Result := False;
    Exit;
  end;
  if OverrideTrayIconSize <> From.OverrideTrayIconSize then
  begin
    Result := False;
    Exit;
  end;
  if OverrideAppIconSize <> From.OverrideAppIconSize then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

initialization;
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
