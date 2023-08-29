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
unit optionsform;

{$mode objfpc}{$H+}{$Q+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, Buttons, Spin, settings, DateUtils,
  {portaudio, }{EventLog,} audio, Math, metronome, log, sound, constants, util;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    bbPlay: TBitBtn;
    bbStop: TBitBtn;
    bbtnDefault: TBitBtn;
    bbtnCancel: TBitBtn;
    bbtnSave: TBitBtn;
    cbAutoProgress: TCheckBox;
    cbModalAlert: TCheckBox;
    cbTrayAlert: TCheckBox;
    cbOverrideTrayIconSize: TCheckBox;
    cbOverrideAppIconSize: TCheckBox;
    cbUseDefaultAudio: TCheckBox;
    cbUseDefaultSound: TCheckBox;
    cbLoopSound: TCheckBox;
    cbOverrideLatency: TCheckBox;
    ckbShowTrayIcon: TCheckBox;
    ckbQueryExit: TCheckBox;
    ckbTimerTitleEditable: TCheckBox;
    cmbTimeFormat: TComboBox;
    dtpCompleteBy: TDateTimePicker;
    dtpDefaultTime: TDateTimePicker;
    dtpShorten: TDateTimePicker;
    edtDefaultDeviceName: TEdit;
    edtDefaultHostAPI: TEdit;
    edtDefaultTitle: TEdit;
    gbApplication: TGroupBox;
    gbIconSizeOverride: TGroupBox;
    gbDefaultsForTimers: TGroupBox;
    gbProgress: TGroupBox;
    gbDefaultAdjustments: TGroupBox;
    gbDevice: TGroupBox;
    gbAudioTest: TGroupBox;
    gbSound: TGroupBox;
    gbVolume: TGroupBox;
    ilOptions: TImageList;
    lblDefaultTitle: TLabel;
    lblCompleteBy: TLabel;
    lblTimeFormat: TLabel;
    lblDefaultTime: TLabel;
    lblCompleteBySuffix: TLabel;
    lblManualSelect: TLabel;
    lblInTaskbar: TLabel;
    lblShorten: TLabel;
    lblVolume: TLabel;
    lsvAudioDevices: TListView;
    pgbAudio: TProgressBar;
    pgcOptions: TPageControl;
    rbProgressOnAppIcon: TRadioButton;
    rbProgressOnOverlayIcon: TRadioButton;
    speTrayIconSize: TSpinEdit;
    speAppIconSize: TSpinEdit;
    seLatency: TSpinEdit;
    tsAlerts: TTabSheet;
    tbVolume: TTrackBar;
    tsAudio: TTabSheet;
    tsTimers: TTabSheet;
    tsInterface: TTabSheet;

    procedure bbPlayClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure bbtnCancelClick(Sender: TObject);
    procedure bbtnDefaultClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure cbOverrideAppIconSizeChange(Sender: TObject);
    procedure cbOverrideLatencyChange(Sender: TObject);
    procedure cbOverrideTrayIconSizeChange(Sender: TObject);
    procedure cbUseDefaultAudioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsvAudioDevicesDblClick(Sender: TObject);
    procedure lsvAudioDevicesItemChecked(Sender: TObject; Item: TListItem);
    procedure pgcOptionsChange(Sender: TObject);
    procedure tbVolumeChange(Sender: TObject);
    procedure tsAudioShow(Sender: TObject);
  private
    { private declarations }
    FLastConfig: TUserConfig;
    FChangedConfig: TUserConfig;
    FDefaultConfig: TUserConfig;
    FTestSound: TSndSound;
    FAudioPlayer: TAudioPlayer;
    FAmpScale: double;
    function GetVolume: integer;
    function GetAmplitudeScale: double;
    procedure RefreshAudioDevices;
    procedure SetControlsAs(Config: TUserConfig);
    procedure GetConfigFromControls(Config: TUserConfig);
    procedure ReenableControls;
    procedure LayoutControls;
  public
    { public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.pgcOptionsChange(Sender: TObject);
begin
  ;
end;

procedure TfrmOptions.tbVolumeChange(Sender: TObject);
begin
  lblVolume.Caption := IntToStr(tbVolume.Position) + '%';
  FAmpScale := AudioSystem.ConvertVolumeToAmplitudeScale(
    Max(Min(tbVolume.Position, MAX_VOLUME), MIN_VOLUME));
end;

procedure TfrmOptions.tsAudioShow(Sender: TObject);
var
  AudioDevice: TAudioDevice;
begin
  if AudioSystem.Loaded then
  begin
    RefreshAudioDevices;
    AudioSystem.GetDefaultDevice(@AudioDevice);
    edtDefaultDeviceName.Text := AudioDevice.DeviceName;
    edtDefaultHostAPI.Text := AudioDevice.HostAPIName;
  end;
end;

procedure TfrmOptions.SetControlsAs(Config: TUserConfig);
var
  Item: TListItem;
begin
  with Config do
  begin
    ckbQueryExit.Checked := QueryExit;
    ckbShowTrayIcon.Checked := ShowTrayIcon;
    ckbTimerTitleEditable.Checked := AllowTimerTitleEdit;

    edtDefaultTitle.Text := DefaultTimerTitle;
    dtpDefaultTime.Time := DefaultTimerDuration;

    cbTrayAlert.Checked := ShowTrayAlert;
    cbModalAlert.Checked := ShowModalAlert;
    cbAutoProgress.Checked := AutoProgress;

    { Time formats tf12 & ts24 translate to 0 and 1, which are the same as
    the indices. Playing with fire, where it can be afforded. }
    cmbTimeFormat.ItemIndex := DefaultTimeFormat;

    dtpShorten.Time := AdjustDiff;
    dtpCompleteBy.Time := AdjustCompleteby;

    cbUseDefaultAudio.Checked := UseDefaultAudioDevice;

    if AudioSystem.Loaded then
    begin
      if (UserConfig.AudioDeviceName <> DEF_AUDIO_DEVICE_NAME) or
        (UserConfig.AudioHostAPIName <> DEF_AUDIO_HOSTAPI_NAME) then
      begin
        for Item in lsvAudioDevices.Items do
        begin
          Item.Checked := ((Item.Caption = UserConfig.AudioDeviceName) and
            (Item.SubItems[LSVADUIO_INDEX_HOSTAPI] = UserConfig.AudioHostAPIName));
        end;

      end;
    end;

    case TaskbarIconType of
      TaskbarAppIcon: rbProgressOnAppIcon.Checked := True;
      TaskbarOverlayIcon: rbProgressOnOverlayIcon.Checked := True;
      else;
    end;

    cbUseDefaultSound.Checked := UseDefaultSound;
    cbLoopSound.Checked := LoopSound;
    tbVolume.Position := AudioSystem.Volume;

    cbOverrideTrayIconSize.Checked := OverrideTrayIconSize;
    cbOverrideAppIconSize.Checked := OverrideAppIconSize;

    speTrayIconSize.Value := TrayIconSizeOverridden;
    speAppIconSize.Value := AppIconSizeOverridden;

    cbOverrideLatency.Checked := OverrideLatency;
    seLatency.Value := Latency;

  end;
end;

procedure TfrmOptions.RefreshAudioDevices;
var
  Devices: TAudioDeviceList;
  Device: PAudioDevice;
  Itm: TListItem;
begin
  tsAudio.Enabled := AudioSystem.Loaded;

  if AudioSystem.Loaded then
  begin
    lsvAudioDevices.Items.Clear;

    Devices := AudioSystem.Devices;
    lsvAudioDevices.Items.BeginUpdate;
    for Device in Devices do
    begin
      Itm := lsvAudioDevices.Items.Add;
      Itm.Caption := Device^.DeviceName;
      Itm.SubItems.Add(Device^.HostAPIName);
      Itm.Checked := ((Itm.Caption = UserConfig.AudioDeviceName) and
        (Itm.SubItems[LSVADUIO_INDEX_HOSTAPI] = UserConfig.AudioHostAPIName));
    end;
    lsvAudioDevices.Items.EndUpdate;
  end;
end;

function TfrmOptions.GetVolume: integer;
begin
  Result := Max(Min(tbVolume.Position, MAX_VOLUME), MIN_VOLUME);
end;

function TfrmOptions.GetAmplitudeScale: double;
begin
  Result := FAmpScale;
end;

procedure TfrmOptions.GetConfigFromControls(Config: TUserConfig);
var
  Item: TListItem;
  DeviceChecked: boolean;
  Device: TAudioDevice;
begin
  with Config do
  begin
    QueryExit := ckbQueryExit.Checked;
    ShowTrayIcon := ckbShowTrayIcon.Checked;
    AllowTimerTitleEdit := ckbTimerTitleEditable.Checked;
    DefaultTimerTitle := edtDefaultTitle.Text;

    DefaultTimerDuration := dtpDefaultTime.Time;
    ShowTrayAlert := cbTrayAlert.Checked;
    ShowModalAlert := cbModalAlert.Checked;
    AutoProgress := cbAutoProgress.Checked;
    DefaultTimeFormat := cmbTimeFormat.ItemIndex;

    AdjustDiff := dtpShorten.Time;

    AdjustCompleteby := dtpCompleteBy.Time;

    UseDefaultAudioDevice := cbUseDefaultAudio.Checked;

    { Find which FAudioPlayer device has been checked }
    DeviceChecked := False;
    for Item in lsvAudioDevices.Items do
    begin
      if Item.Checked then
      begin
        AudioDeviceName := Item.Caption;
        AudioHostAPIName := Item.SubItems[LSVADUIO_INDEX_HOSTAPI];
        DeviceChecked := True;
        Break;
      end;
    end;

    { If no item was checked, put the default device}
    if not DeviceChecked then
    begin
      AudioSystem.GetDefaultDevice(@Device);
      AudioDeviceName := Device.DeviceName;
      AudioHostAPIName := Device.HostAPIName;
    end;

    if rbProgressOnOverlayIcon.Checked then
      TaskbarIconType := TaskbarOverlayIcon
    else if rbProgressOnAppIcon.Checked then
      TaskbarIconType := TaskbarAppIcon;

    UseDefaultSound := cbUseDefaultSound.Checked;
    LoopSound := cbLoopSound.Checked;
    AudioSystem.Volume := Max(Min(tbVolume.Position, MAX_VOLUME), MIN_VOLUME);
    Volume := AudioSystem.Volume;

    OverrideTrayIconSize := cbOverrideTrayIconSize.Checked;
    OverrideAppIconSize := cbOverrideAppIconSize.Checked;

    TrayIconSizeOverridden := speTrayIconSize.Value;
    AppIconSizeOverridden := speAppIconSize.Value;

    OverrideLatency := cbOverrideLatency.Checked;
    Latency := seLatency.Value;

  end;
end;

procedure TfrmOptions.ReenableControls;
begin
  speTrayIconSize.Enabled := cbOverrideTrayIconSize.Checked;
  speAppIconSize.Enabled := cbOverrideAppIconSize.Checked;
  tbVolume.Enabled := AudioSystem.Loaded;
  cbUseDefaultAudio.Enabled := AudioSystem.Loaded;
  seLatency.Enabled := cbOverrideLatency.Checked;

  {$IF not defined(windows)}
  rbProgressOnAppIcon.Checked := True;
  rbProgressOnOverlayIcon.Checked := False;
  rbProgressOnOverlayIcon.Enabled := False;
  {$ENDIF}
end;

procedure TfrmOptions.LayoutControls;
var
  WidgetControls: TControlList;
  WidgetControl: TControl;
  MinWidth: integer = 0;
begin

  { The labels in each tabsheet needs to be right aligned. For this, the
  text extent of each of these labels are obtained prior, a buffer is added and
  then the minimum width for each is set to that. This ensures that there is
  no underflow }
  WidgetControls := TControlList.Create;

  with WidgetControls do
  begin
    Add(lblDefaultTitle);
    Add(lblDefaultTime);
    Add(lblShorten);
    Add(lblCompleteBy);
  end;

  for WidgetControl in WidgetControls do
    MinWidth := Max(Canvas.TextWidth(WidgetControl.Caption), MinWidth);

  Inc(MinWidth, 2 * UserInterfaceMetrics.Margin);
  for WidgetControl in WidgetControls do
    WidgetControl.Constraints.MinWidth := MinWidth;

  WidgetControls.Free;


  with UserInterfaceMetrics do
  begin
    with bbtnSave.BorderSpacing do
    begin
      Right := Margin;
      Bottom := Margin;
    end;
    bbtnCancel.BorderSpacing.Right := Padding;
    bbtnDefault.BorderSpacing.Left := Margin;

    with pgcOptions.BorderSpacing do
    begin
      Top := Margin;
      Bottom := Padding;
    end;

    {Tab: Timers}

    gbDefaultsForTimers.BorderSpacing.Around := Padding;
    gbDefaultsForTimers.BorderSpacing.InnerBorder := Padding;
    gbDefaultAdjustments.BorderSpacing.Bottom := Padding;
    gbDefaultAdjustments.BorderSpacing.InnerBorder := Padding;

    with edtDefaultTitle.BorderSpacing do
    begin
      Top := Padding;
      Right := Padding;
      Left := Padding;
    end;

    lblDefaultTitle.BorderSpacing.Left := Padding;

    with dtpDefaultTime.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
    end;

    with dtpShorten.BorderSpacing do
    begin
      Top := Margin;
      Right := Margin;
      Left := Padding;
    end;
    lblShorten.BorderSpacing.Left := Padding;
    dtpCompleteBy.BorderSpacing.Top := Padding;

    lblCompleteBySuffix.BorderSpacing.Left := Padding;

    {Tab: Alerts}
    with gbProgress.BorderSpacing do
    begin
      Around := Padding;
      InnerBorder := Padding;
    end;

    with gbSound.BorderSpacing do
    begin
      Bottom := Padding;
      InnerBorder := Padding;
    end;

    with cbTrayAlert.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
    end;

    cbModalAlert.BorderSpacing.Top := Padding;
    cbAutoProgress.BorderSpacing.Top := Padding;
    lblInTaskbar.BorderSpacing.Top := (Padding);

    with rbProgressOnAppIcon.BorderSpacing do
    begin
      Top := Padding;
      Left := (2 * Padding);
    end;
    rbProgressOnOverlayIcon.BorderSpacing.Top := Padding;

    with cbUseDefaultSound.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
    end;

    cbLoopSound.BorderSpacing.Top := Padding;

    { Tab: interface }

    with gbApplication.BorderSpacing do
    begin
      Around := Padding;
      InnerBorder := Padding;
    end;

    gbIconSizeOverride.BorderSpacing.InnerBorder := Padding;

    with ckbQueryExit.BorderSpacing do
    begin
      Left := Padding;
      Top := Padding;
    end;

    ckbShowTrayIcon.BorderSpacing.Top := Padding;
    ckbTimerTitleEditable.BorderSpacing.Top := Padding;
    cmbTimeFormat.BorderSpacing.Top := Padding;

    cbOverrideTrayIconSize.BorderSpacing.Left := Padding;
    speTrayIconSize.BorderSpacing.Top := Padding;
    speAppIconSize.BorderSpacing.Top := Padding;
    speAppIconSize.BorderSpacing.Left := Padding;

    { Tab: audio }

    //tsAudio.BorderSpacing.InnerBorder:=Margin;

    with gbAudioTest.BorderSpacing do
    begin
      Around := Padding;
      InnerBorder := Padding;
      //Right := Padding;
      //Right := Padding;
    end;

    gbVolume.BorderSpacing.Left := Padding;

    with edtDefaultHostAPI.BorderSpacing do
    begin
      Top := Padding;
      Right := Padding;
    end;

    with edtDefaultDeviceName.BorderSpacing do
    begin
      Left := Padding;
      Right := Padding;
    end;
    cbUseDefaultAudio.BorderSpacing.Left := Padding;
    lblManualSelect.BorderSpacing.Top := Padding;
    lsvAudioDevices.BorderSpacing.Top := Padding;
    seLatency.BorderSpacing.Top := Padding;
    seLatency.BorderSpacing.Left := Padding;
    SeLatency.BorderSpacing.Bottom := Padding;

    gbDevice.BorderSpacing.InnerBorder := Padding;
    gbDevice.BorderSpacing.Bottom := Padding;


    //lblVolume.BorderSpacing.Left:=Margin;
    with tbVolume.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
      Right := Padding;
    end;

    with bbStop.BorderSpacing do
    begin
      Top := Padding;
      Right := Padding;
    end;

    bbPlay.BorderSpacing.Right := Padding;

    with pgbAudio.BorderSpacing do
    begin
      Left := Padding;
      Right := Padding;
    end;

    //gbAudioTest.BorderSpacing.Bottom := Padding;

  end;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  AudioDevice: TAudioDevice;
begin
  FTestSound := nil;

  OnShow := @FormShow;
  FLastConfig := TUserConfig.Create;
  FChangedConfig := TUserConfig.Create;
  FDefaultConfig := TUserConfig.Create;
  SetControlsAs(UserConfig);
  pgcOptions.ActivePage := tsTimers;
  FAudioPlayer := nil;
  bbStop.Enabled := False;

  FAmpScale := AudioSystem.AmplitudeScale;

  if AudioSystem.Loaded then
  begin
    RefreshAudioDevices;
    AudioSystem.GetDefaultDevice(@AudioDevice);
    edtDefaultDeviceName.Text := AudioDevice.DeviceName;
    edtDefaultHostAPI.Text := AudioDevice.HostAPIName;
    FAudioPlayer := TAudioPlayer.Create;
    FAudioPlayer.Looped := True;
    bbPlay.Enabled := True;

    FTestSound := TSndSound.Create;
  end
  else
  begin
    edtDefaultDeviceName.Text := 'Audio libraries not loaded. Audio will not work';
    edtDefaultHostAPI.Text := '';
    bbPlay.Enabled := False;
  end;

  seLatency.MinValue := AUDIO_LATENCY_MIN;
  seLatency.MaxValue := AUDIO_LATENCY_MAX;

  LayoutControls;

  ReenableControls;

end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FTestSound.Free;
  FAudioPlayer.Free;
  FLastConfig.Free;
  FChangedConfig.Free;
  FDefaultConfig.Free;
end;

procedure TfrmOptions.FormHide(Sender: TObject);
var
  StartTickCount: QWord;
begin
  if AudioSystem.Loaded and FAudioPlayer.Playing then
  begin

    StartTickCount := GetTickCount64;
    { Wait for FAudio to complete }
    if FAudioPlayer.Playing then
    begin
      FAudioPlayer.Abort;
      while FAudioPlayer.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
          break;
      end;
    end;

    { As there is no audiocompleted completed, it is okay not to wait }
    pgbAudio.Style := pbstNormal;
    bbPlay.Enabled := True;
    bbStop.Enabled := False;
  end;
end;

procedure TfrmOptions.bbtnCancelClick(Sender: TObject);
begin
  GetConfigFromControls(FChangedConfig);
  if not FChangedConfig.CompareWith(FLastConfig) then
  begin
    if MessageDlg('Confirmation',
      'You have unsaved changes. Press Ok to proceed and lose changes.',
      mtInformation, mbOKCancel, 0) = mrCancel then
    begin
      Exit;
    end;
  end;
  SetControlsAs(FLastConfig);
  Close;

end;

procedure TfrmOptions.bbPlayClick(Sender: TObject);
var
  Item: TListItem;
  Device: TAudioDevice;
begin
  if AudioSystem.Loaded then
  begin
    AudioSystem.UseDefaultDevice := cbUseDefaultAudio.Checked;

    if not AudioSystem.UseDefaultDevice then
    begin
      AudioSystem.SetDefaulDevice; { In case no items are checked }
      for Item in lsvAudioDevices.Items do
      begin
        if Item.Checked then
        begin
          Device.DeviceName := Item.Caption;
          Device.HostAPIName := Item.SubItems[LSVADUIO_INDEX_HOSTAPI];
          AudioSystem.OutputDevice := Device;
        end;
      end;
    end;

    FAudioPlayer.AmplitudeScalePoller := @GetAmplitudeScale;
    FAudioPlayer.Play(SoundPool.RawDefaultSound);

    pgbAudio.Style := pbstMarquee;
    bbPlay.Enabled := False;
    bbStop.Enabled := True;
  end;
end;

procedure TfrmOptions.bbStopClick(Sender: TObject);
var
  StartTickCount: QWord;
begin
  if AudioSystem.Loaded then;
  begin
    StartTickCount := GetTickCount64;
    { Wait for FAudio to complete }
    if FAudioPlayer.Playing then
    begin
      FAudioPlayer.Abort;
      while FAudioPlayer.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
          break;
      end;
    end;
    pgbAudio.Style := pbstNormal;
    bbPlay.Enabled := True;
    bbStop.Enabled := False;

  end;
end;

procedure TfrmOptions.bbtnDefaultClick(Sender: TObject);
begin
  GetConfigFromControls(FChangedConfig);
  if not FChangedConfig.CompareWith(FDefaultConfig) then
  begin
    if MessageDlg('Confirmation', 'Reset options to default?',
      mtInformation, mbOKCancel, 0) = mrOk then
    begin
      SetControlsAs(FDefaultConfig);
    end;
  end;

end;

procedure TfrmOptions.bbtnSaveClick(Sender: TObject);
{var
  Audio: TAudioPlayer;}
begin
  GetConfigFromControls(UserConfig);
  UserConfig.Flush;

  Close;
end;

procedure TfrmOptions.cbOverrideAppIconSizeChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmOptions.cbOverrideLatencyChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmOptions.cbOverrideTrayIconSizeChange(Sender: TObject);
begin
  ReenableControls;
end;

procedure TfrmOptions.cbUseDefaultAudioChange(Sender: TObject);
begin
  UserConfig.UseDefaultAudioDevice := cbUseDefaultAudio.Checked;
  lsvAudioDevices.Enabled := (not cbUseDefaultAudio.Checked);

end;


procedure TfrmOptions.FormShow(Sender: TObject);
begin
  SetControlsAs(UserConfig);
  FLastConfig.CopyFrom(UserConfig);
end;

procedure TfrmOptions.lsvAudioDevicesDblClick(Sender: TObject);
var
  hts: THitTests;
  CurrPos: TPoint;

  Selected, AnItem: TListItem;
begin
  { Get the position of the mouse cursor related to ListView }
  CurrPos := lsvAudioDevices.ScreenToClient(Mouse.CursorPos);

  { Where was the double-click received? }
  hts := lsvAudioDevices.GetHitTestInfoAt(CurrPos.X, CurrPos.Y);
  if hts <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon] then
  begin
    Selected := lsvAudioDevices.Selected;
    for AnItem in lsvAudioDevices.Items do
      AnItem.Checked := (Selected = Anitem);
  end;
end;

procedure TfrmOptions.lsvAudioDevicesItemChecked(Sender: TObject; Item: TListItem);
var
  AnItem: TListitem;
begin
  for AnItem in lsvAudioDevices.Items do
  begin
    if Anitem <> item then
      AnItem.Checked := False;
  end;
end;


end.
