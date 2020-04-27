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
unit optionsform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DateTimePicker, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, Buttons, Spin, settings, DateUtils,
  {portaudio, }EventLog, audio, Math;

const
  LSVADUIO_INDEX_HOSTAPI: integer = 0;

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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    cbUseDefaultAudio: TCheckBox;
    cbUseDefaultSound: TCheckBox;
    cbLoopSound: TCheckBox;
    CheckBox4: TCheckBox;
    ckbQueryExit: TCheckBox;
    ckbTimerTitleEditable: TCheckBox;
    cmbTimeFormat: TComboBox;
    dtpCompleteBy: TDateTimePicker;
    dtpDefaultTime: TDateTimePicker;
    dtpShorten: TDateTimePicker;
    edtDefaultDeviceName: TEdit;
    edtDefaultHostAPI: TEdit;
    edtDefaultTitle: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Defaults: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    ilOptions: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lsvAudioDevices: TListView;
    pgbAudio: TProgressBar;
    pgcOptions: TPageControl;
    rbProgressOnAppIcon: TRadioButton;
    rbProgressOnOverlayIcon: TRadioButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    speBpm: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tbVolume: TTrackBar;
    tsAudio: TTabSheet;
    tsTimers: TTabSheet;
    tsInterface: TTabSheet;

    procedure bbPlayClick(Sender: TObject);
    procedure bbStopClick(Sender: TObject);
    procedure bbtnCancelClick(Sender: TObject);
    procedure bbtnDefaultClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure cbUseDefaultAudioChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lsvAudioDevicesDblClick(Sender: TObject);
    procedure lsvAudioDevicesItemChecked(Sender: TObject; Item: TListItem);
    procedure pgcOptionsChange(Sender: TObject);
    procedure tsAudioShow(Sender: TObject);
  private
    { private declarations }
    FLastConfig: TUserConfig;
    FChangedConfig: TUserConfig;
    FDefaultConfig: TUserConfig;
    FTestSound: TSndSound;
    Audio: TAudio;
    function GetVolume: integer;
    procedure RefreshAudioDevices;
    procedure SetControlsAs(Config: TUserConfig);
    procedure GetConfigFromControls(Config: TUserConfig);
    procedure SetVolume(AValue: integer);
  public
    { public declarations }
    //property Volume: integer read GetVolume write SetVolume;
  end;

var
  frmOptions: TfrmOptions;

implementation

uses
  main;

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.pgcOptionsChange(Sender: TObject);
begin

end;

procedure TfrmOptions.tsAudioShow(Sender: TObject);
var
  AudioDevice: TAudioDevice;
begin
  if TAudio.Loaded then
  begin
    RefreshAudioDevices;
    TAudio.GetDefaultDevice(@AudioDevice);
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
    ckbTimerTitleEditable.Checked := AllowTimerTitleEdit;

    edtDefaultTitle.Text := DefaultTimerTitle;
    dtpDefaultTime.Time := DefaultTimerDuration;

    cbTrayAlert.Checked := ShowTrayAlert;
    cbModalAlert.Checked := ShowModalAlert;
    cbAutoProgress.Checked := AutoProgress;

    { Time formats tf12 & ts24 translate to 0 and 1, which are the same as
    the indices. Playing with fire, where it can be afforded. }
    cmbTimeFormat.ItemIndex := DefaultTimeFormat;

    dtpShorten.Time := AdjustDiffDefault;
    dtpCompleteBy.Time := AdjustCompletebyDefault;

    cbUseDefaultAudio.Checked := UseDefaultAudioDevice;

    if TAudio.Loaded then
    begin
      if (GlobalUserConfig.AudioDeviceName <> DEF_AUDIO_DEVICE_NAME) or
        (GlobalUserConfig.AudioHostAPIName <> DEF_AUDIO_HOSTAPI_NAME) then
      begin
        for Item in lsvAudioDevices.Items do
        begin
          Item.Checked := ((Item.Caption = GlobalUserConfig.AudioDeviceName) and
            (Item.SubItems[LSVADUIO_INDEX_HOSTAPI] = GlobalUserConfig.AudioHostAPIName));
        end;

      end;
    end;

    case TaskbarIconType of
      TaskbarAppIcon: rbProgressOnAppIcon.Checked := True;
      TaskbarOverlayIcon: rbProgressOnOverlayIcon.Checked := True;
    end;

    cbUseDefaultSound.Checked := UseDefaultSound;
    cbLoopSound.Checked := LoopSound;
    tbVolume.Position:=Volume;

  end;
end;

procedure TfrmOptions.RefreshAudioDevices;
var
  Devices: TAudioDeviceList;
  Device: PAudioDevice;
  Itm: TListItem;
begin
  tsAudio.Enabled := TAudio.Loaded;

  if TAudio.Loaded then
  begin
    lsvAudioDevices.Items.Clear;

    Devices := TAudio.Devices;
    lsvAudioDevices.Items.BeginUpdate;
    for Device in Devices do
    begin
      Itm := lsvAudioDevices.Items.Add;
      Itm.Caption := Device^.DeviceName;
      Itm.SubItems.Add(Device^.HostAPIName);
      Itm.Checked := ((Itm.Caption = GlobalUserConfig.AudioDeviceName) and
        (Itm.SubItems[LSVADUIO_INDEX_HOSTAPI] = GlobalUserConfig.AudioHostAPIName));
    end;
    lsvAudioDevices.Items.EndUpdate;
  end;
end;

function TfrmOptions.GetVolume: integer;
begin
  Result := Min(tbVolume.Position, MAX_VOLUME);
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
    AllowTimerTitleEdit := ckbTimerTitleEditable.Checked;
    DefaultTimerTitle := edtDefaultTitle.Text;

    DefaultTimerDuration := dtpDefaultTime.Time;
    ShowTrayAlert := cbTrayAlert.Checked;
    ShowModalAlert := cbModalAlert.Checked;
    AutoProgress := cbAutoProgress.Checked;
    DefaultTimeFormat := cmbTimeFormat.ItemIndex;

    AdjustDiffDefault := dtpShorten.Time;

    AdjustCompletebyDefault := dtpCompleteBy.Time;

    UseDefaultAudioDevice := cbUseDefaultAudio.Checked;

    { Find which audio device has been checked }
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
      TAudio.GetDefaultDevice(@Device);
      AudioDeviceName := Device.DeviceName;
      AudioHostAPIName := Device.HostAPIName;
    end;

    if rbProgressOnOverlayIcon.Checked then
      TaskbarIconType := TaskbarOverlayIcon
    else if rbProgressOnAppIcon.Checked then
      TaskbarIconType := TaskbarAppIcon;

    UseDefaultSound := cbUseDefaultSound.Checked;
    LoopSound := cbLoopSound.Checked;
    Volume := tbVolume.Position;

  end;
end;

procedure TfrmOptions.SetVolume(AValue: integer);
begin
  tbVolume.Position:=Min(AValue, MAX_VOLUME);
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
  SetControlsAs(GlobalUserConfig);
  pgcOptions.ActivePage := tsTimers;
  Audio := nil;
  bbStop.Enabled := False;
  if TAudio.Loaded then
  begin
    RefreshAudioDevices;
    TAudio.GetDefaultDevice(@AudioDevice);
    edtDefaultDeviceName.Text := AudioDevice.DeviceName;
    edtDefaultHostAPI.Text := AudioDevice.HostAPIName;
    Audio := TAudio.Create;
    bbPlay.Enabled := True;

    FTestSound := TSndSound.Create;
    FTestSound.LoadDefaultSound;

  end
  else
  begin
    edtDefaultDeviceName.Text := 'Audio libraries not loaded. Audio will not work';
    edtDefaultHostAPI.Text := '';
    bbPlay.Enabled := False;
  end;

end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FTestSound.Free;
  Audio.Free;
  FLastConfig.Free;
  FChangedConfig.Free;
  FDefaultConfig.Free;
end;

procedure TfrmOptions.FormHide(Sender: TObject);
var
  StartTickCount: longword;
begin
  if TAudio.Loaded and Audio.Playing then
  begin

    StartTickCount := GetTickCount64;
    { Wait for FAudio to complete }
    if Audio.Playing then
    begin
      Audio.Abort;
      while Audio.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        //TODO: Remove hardcoding
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
begin
  if TAudio.Loaded then
  begin
    TAudio.UseDefaultDevice := cbUseDefaultAudio.Checked;

    if not TAudio.UseDefaultDevice then
    begin
      TAudio.SetDefaulDevice; // In case no items are checked
      for Item in lsvAudioDevices.Items do
      begin
        if Item.Checked then
        begin
          // TODO: FOutpuDevice should be renamed?
          TAudio.FOutputDevice.DeviceName := Item.Caption;
          TAudio.FOutputDevice.HostAPIName := Item.SubItems[LSVADUIO_INDEX_HOSTAPI];
        end;
      end;


    end;

    //Audio.PlaySine;
    //Audio.PlayTest;

    Audio.Play(FTestSound, True, tbVolume.Position);

    pgbAudio.Style := pbstMarquee;
    bbPlay.Enabled := False;
    bbStop.Enabled := True;
  end;
end;

procedure TfrmOptions.bbStopClick(Sender: TObject);
var
  StartTickCount: longword;
begin
  if TAudio.Loaded then;
  begin
    StartTickCount := GetTickCount64;
    { Wait for FAudio to complete }
    if Audio.Playing then
    begin
      Audio.Abort;
      while Audio.Playing do
      begin
        Logger.Debug('Waiting for');
        Application.ProcessMessages;
        //TODO: Remove hardcoding
        if GetTickCount64 > (StartTickCount + AUDIO_ABORT_SHORT_WAIT) then
          break;
      end;
    end;
    //Audio.Abort;
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
      //Close;
    end;
  end;

end;

procedure TfrmOptions.bbtnSaveClick(Sender: TObject);
{var
  Audio: TAudio;}
begin
  GetConfigFromControls(GlobalUserConfig);
  GlobalUserConfig.Flush;

  Close;
end;

procedure TfrmOptions.cbUseDefaultAudioChange(Sender: TObject);
begin
  GlobalUserConfig.UseDefaultAudioDevice := cbUseDefaultAudio.Checked;
  lsvAudioDevices.Enabled := (not cbUseDefaultAudio.Checked);

end;


procedure TfrmOptions.FormShow(Sender: TObject);
begin
  SetControlsAs(GlobalUserConfig);
  FLastConfig.CopyFrom(GlobalUserConfig);
end;

procedure TfrmOptions.lsvAudioDevicesDblClick(Sender: TObject);
var
  hts: THitTests;
  //ht : THitTest;
  //sht : string;
  CurrPos: TPoint;

  Selected, AnItem: TListItem;
begin
  // Get the position of the mouse cursor related to ListView
  CurrPos := lsvAudioDevices.ScreenToClient(Mouse.CursorPos);

  // Where was the double-click received?
  hts := lsvAudioDevices.GetHitTestInfoAt(CurrPos.X, CurrPos.Y);
  if hts <= [htOnIcon, htOnItem, htOnLabel, htOnStateIcon] then
  begin
    Selected := lsvAudioDevices.Selected;
    //Selected.Checked:=True;
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
