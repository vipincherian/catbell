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
  {portaudio, }LazLogger, audio;

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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    cbUseDefaultAudio: TCheckBox;
    ckbQueryExit: TCheckBox;
    cbTrayAlert: TCheckBox;
    cbModalAlert: TCheckBox;
    cbAutoProgress: TCheckBox;
    ckbTimerTitleEditable: TCheckBox;
    cmbAudioDevice: TComboBox;
    cmbTimeFormat: TComboBox;
    dtpCompleteBy: TDateTimePicker;
    dtpDefaultTime: TDateTimePicker;
    dtpShorten: TDateTimePicker;
    edtDefaultTitle: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Defaults: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    ilOptions: TImageList;
    Label1: TLabel;
    lblDefaultDeviceName: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lsvAudioDevices: TListView;
    pgbAudio: TProgressBar;
    pgcOptions: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
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
    procedure pgcOptionsChange(Sender: TObject);
    procedure tsAudioShow(Sender: TObject);
  private
    { private declarations }
    FLastConfig: TUserConfig;
    FChangedConfig: TUserConfig;
    FDefaultConfig: TUserConfig;
    Audio: TAudio;
    procedure RefreshAudioDevices;
    procedure SetControlsAs(Config: TUserConfig);
    procedure GetConfigFromControls(Config: TUserConfig);
  public
    { public declarations }
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
    lblDefaultDeviceName.Caption :=
      AudioDevice.DeviceName + ' - ' + AudioDevice.HostAPIName;
  end;
end;

procedure TfrmOptions.SetControlsAs(Config: TUserConfig);
var
  Index: integer;
  DefaultDeviceId: integer;
  Item: TListItem;
begin
  with Config do
  begin
    ckbQueryExit.Checked := QueryExit;
    ckbTimerTitleEditable.Checked := AllowTimerTitleEdit;

    edtDefaultTitle.Text := DefaultTimerTitle;
    dtpDefaultTime.Time := DefaultTimerDuration;
    //EncodeTime(DefaultTimerHours, DefaultTimerMins,
    //      DefaultTimerSecs, 0);

    cbTrayAlert.Checked := ShowTrayAlert;
    cbModalAlert.Checked := ShowModalAlert;
    cbAutoProgress.Checked := AutoProgress;

    { Time formats tf12 & ts24 translate to 0 and 1, which are the same as
    the indices. Playing with fire, where it can be afforded. }
    cmbTimeFormat.ItemIndex := DefaultTimeFormat;

    dtpShorten.Time := AdjustDiffDefault;
    //dtpExtend.Time:=AdjustExtendDefault;
    dtpCompleteBy.Time := AdjustCompletebyDefault;

    cbUseDefaultAudio.Checked:=UseDefaultAudioDevice;

    if TAudio.Loaded then
    begin
      if (GlobalUserConfig.AudioDeviceName <> DEF_AUDIO_DEVICE_NAME) or
        (GlobalUserConfig.AudioHostAPIName <> DEF_AUDIO_HOSTAPI_NAME) then
      begin
        Index := cmbAudioDevice.Items.IndexOf(GlobalUserConfig.AudioDeviceName);
        if index >= 0 then
          cmbAudioDevice.ItemIndex :=
            cmbAudioDevice.Items.IndexOf(GlobalUserConfig.AudioDeviceName)
        else
        begin
          //DefaultDeviceId := TAudio.GetDefaultDeviceIndex; //Pa_GetDefaultOutputDevice();
          {if DefaultDevice = paNoDevice then
          begin
            DebugLn('No default device');
            //tsAudio.Enabled:=False;
            cmbAudioDevice.Enabled := False;
          end
          else}
          cmbAudioDevice.ItemIndex := TAudio.DefaultDeviceIndex;
        end;

        for Item in lsvAudioDevices.Items do
        begin
          Item.Checked := ((Item.Caption = GlobalUserConfig.AudioDeviceName) and
            (Item.SubItems[LSVADUIO_INDEX_HOSTAPI] = GlobalUserConfig.AudioHostAPIName));
          //Item.Checked:=True;
        end;

      end;
    end;

  end;
end;

procedure TfrmOptions.RefreshAudioDevices;
var
  DefaultDeviceId: AudioDeviceIndex;
  Devices: TAudioDeviceList;
  Device: PAudioDevice;
  Count: integer;
  Itm: TListItem;
begin
  { Load audio devices }
  //tsAudio.Enabled:=frmMain.AudioWorking;
  tsAudio.Enabled := TAudio.Loaded;

  if TAudio.Loaded then
  begin
    cmbAudioDevice.Items.Clear;
    lsvAudioDevices.Items.Clear;
    DefaultDeviceId := TAudio.GetDefaultDeviceIndex;
    Devices := TAudio.Devices;
    for Device in Devices do
    begin
      //Device := Devices.Strings[Count];
      {if Count = DefaultDeviceId then
        Device := Device + ' (Default)';}
      cmbAudioDevice.Items.Add(Device^.DeviceName);
      Itm := lsvAudioDevices.Items.Add;
      Itm.Caption := Device^.DeviceName;
      Itm.SubItems.Add(Device^.HostAPIName);
      Itm.Checked := ((Itm.Caption = GlobalUserConfig.AudioDeviceName) and
        (Itm.SubItems[LSVADUIO_INDEX_HOSTAPI] = GlobalUserConfig.AudioHostAPIName));
    end;

    if cmbAudioDevice.Items.Count > 0 then
    begin
      if GlobalUserConfig.AudioDeviceName = DEF_AUDIO_DEVICE_NAME then
        cmbAudioDevice.ItemIndex := TAudio.DefaultDeviceIndex
      else
        cmbAudioDevice.ItemIndex :=
          cmbAudioDevice.Items.IndexOf(GlobalUserConfig.AudioDeviceName);
    end;
  end;
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
    //DefaultTimerHours := HourOf(dtpDefaultTime.Time);
    //DefaultTimerMins := MinuteOf(dtpDefaultTime.Time);
    //DefaultTimerSecs := SecondOf(dtpDefaultTime.Time);
    DefaultTimerDuration := dtpDefaultTime.Time;
    ShowTrayAlert := cbTrayAlert.Checked;
    ShowModalAlert := cbModalAlert.Checked;
    AutoProgress := cbAutoProgress.Checked;
    DefaultTimeFormat := cmbTimeFormat.ItemIndex;

    AdjustDiffDefault := dtpShorten.Time;
    //AdjustExtendDefault:=dtpExtend.Time;
    AdjustCompletebyDefault := dtpCompleteBy.Time;

    //AudioDevice := cmbAudioDevice.ItemIndex;
    if TAudio.Loaded and (cmbAudioDevice.ItemIndex >= 0) then
      AudioDeviceName := cmbAudioDevice.Items.Strings[cmbAudioDevice.ItemIndex];

    Config.UseDefaultAudioDevice:=cbUseDefaultAudio.Checked;

    { Find which audio device has been checked }
    DeviceChecked := False;
    for Item in lsvAudioDevices.Items do
    begin
      if Item.Checked then
      begin
        AudioDeviceName:=Item.Caption;
        AudioHostAPIName:=Item.SubItems[LSVADUIO_INDEX_HOSTAPI];
        DeviceChecked := True;
        Break;
      end;
    end;

    { If no item was checked, put the default device}
    if not DeviceChecked then
    begin
      TAudio.GetDefaultDevice(@Device);
      AudioDeviceName:=Device.DeviceName;
      AudioHostAPIName:=Device.HostAPIName;
    end;

  end;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  AudioDevice: TAudioDevice;
begin
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
    lblDefaultDeviceName.Caption :=
      AudioDevice.DeviceName + ' - ' + AudioDevice.HostAPIName;
    Audio := TAudio.Create;
    bbPlay.Enabled := True;
  end
  else
  begin
    cmbAudioDevice.Enabled := False;
    lblDefaultDeviceName.Caption := 'Audio libraries not loaded. Audio will not work';
    bbPlay.Enabled := False;
  end;

end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  Audio.Free;
  FLastConfig.Free;
  FChangedConfig.Free;
  FDefaultConfig.Free;
end;

procedure TfrmOptions.FormHide(Sender: TObject);
begin
  if TAudio.Loaded and Audio.Playing then
  begin
    Audio.Abort;
    pgbAudio.Style := pbstNormal;
    bbPlay.Enabled := True;
    bbStop.Enabled := False;
    {while Audio.Playing do
      Application.ProcessMessages;}
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

    for Item in lsvAudioDevices.Items do
    begin
      if Item.Checked then
      begin
        // TODO: FOutpuDevice should be renamed?
        TAudio.FOutputDevice.DeviceName := Item.Caption;
        TAudio.FOutputDevice.HostAPIName:=Item.SubItems[LSVADUIO_INDEX_HOSTAPI];
        Audio.PlaySine;
        pgbAudio.Style := pbstMarquee;
        bbPlay.Enabled := False;
        bbStop.Enabled := True;
      end;
    end;

    {if cmbAudioDevice.ItemIndex >= 0 then
    begin

      //Audio.OutputDevice := cmbAudioDevice.Items[cmbAudioDevice.ItemIndex];
      TAudio.SetDefaulDevice;
      //ShowMessage(Audio.OutputDevice);
      Audio.PlaySine;
      pgbAudio.Style := pbstMarquee;
      bbPlay.Enabled := False;
      bbStop.Enabled := True;
    end;}

  end;
end;

procedure TfrmOptions.bbStopClick(Sender: TObject);
begin
  if TAudio.Loaded then;
  begin
    Audio.Abort;
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


  {ckbQueryExit.Checked := DEF_QUERY_EXIT;

  edtDefaultTitle.Text := DEF_TIMER_TITLE;
  dtpDefaultTime.Time := EncodeTime(DEF_TIMER_HOURS, DEF_TIMER_MINS,
    DEF_TIMER_SECS, 0);

  cbTrayAlert.Checked := DEF_SHOW_TRAY_ALERT;
  cbModalAlert.Checked := DEF_SHOW_MODAL_ALERT;
  cbAutoProgress.Checked := DEF_AUTO_PROGRESS;

  crbBackgroundModal.ButtonColor:=DEF_MODAL_BKG_COLOUR;
  crbCaptionModal.ButtonColor:=DEF_MODAL_CAPTION_COLOUR;
  crbSubtextModal.ButtonColor:=DEF_MODAL_SUB_COLOUR;}

end;

procedure TfrmOptions.bbtnSaveClick(Sender: TObject);
{var
  Audio: TAudio;}
begin
  {with GlobalUserConfig do
  begin
    QueryExit := ckbQueryExit.Checked;
    DefaultTimerTitle := edtDefaultTitle.Text;
    DefaultTimerHours := HourOf(dtpDefaultTime.Time);
    DefaultTimerMins := MinuteOf(dtpDefaultTime.Time);
    DefaultTimerSecs := SecondOf(dtpDefaultTime.Time);
    ShowTrayAlert := cbTrayAlert.Checked;
    ShowModalAlert := cbModalAlert.Checked;
    AutoProgress := cbAutoProgress.Checked;
    ModalBackgroundColour:=crbBackgroundModal.ButtonColor;
    ModalCaptionColour:=crbCaptionModal.ButtonColor;
    ModalSubtextColour:=crbSubtextModal.ButtonColor;
  end;}
  GetConfigFromControls(GlobalUserConfig);
  //  GlobalUserConfig.CopyFrom(FChangedConfig);
  GlobalUserConfig.Flush;

  {Audio := TAudio.Create;
  Audio.FileName := '/media/data/down/www/just-like-magic.ogg';
  DebugLn('File is ' + Audio.FileName);
  Audio.Play;
  Sleep(3000);
  Audio.Free;}
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


end.
