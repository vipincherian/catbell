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
  Dialogs, ComCtrls, StdCtrls, Buttons, Spin, settings, DateUtils, portaudio, LazLogger;

type

  { TfrmOptions }

  TfrmOptions = class(TForm)
    bbtnDefault: TBitBtn;
    bbtnCancel: TBitBtn;
    bbtnSave: TBitBtn;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ckbQueryExit: TCheckBox;
    cbTrayAlert: TCheckBox;
    cbModalAlert: TCheckBox;
    cbAutoProgress: TCheckBox;
    ckbTimerTitleEditable: TCheckBox;
    cmbTimeFormat: TComboBox;
    cmbAudioDevice: TComboBox;
    dtpShorten: TDateTimePicker;
    dtpCompleteBy: TDateTimePicker;
    dtpDefaultTime: TDateTimePicker;
    edtDefaultTitle: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Defaults: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    ilOptions: TImageList;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    pgcOptions: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    tsAdjust: TTabSheet;
    tsAudio: TTabSheet;
    tsTimers: TTabSheet;
    tsInterface: TTabSheet;

    procedure bbtnCancelClick(Sender: TObject);
    procedure bbtnDefaultClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pgcOptionsChange(Sender: TObject);
  private
    { private declarations }
    FLastConfig: TUserConfig;
    FChangedConfig: TUserConfig;
    FDefaultConfig: TUserConfig;
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

procedure TfrmOptions.SetControlsAs(Config: TUserConfig);
var
  Index: integer;
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

    if GlobalUserConfig.AudioDeviceName <> DEF_AUDIO_DEVICE_NAME then
    begin
      cmbAudioDevice.ItemIndex:=cmbAudioDevice.Items.IndexOf(GlobalUserConfig.AudioDeviceName);
    end;
  end;
end;

procedure TfrmOptions.GetConfigFromControls(Config: TUserConfig);
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
    AudioDeviceName:=cmbAudioDevice.Items.Strings[cmbAudioDevice.ItemIndex];

  end;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
var
  //PaErrCode: PaError;
  NumDevices, DefaultDevice, Count: integer;
  DeviceInfo: PPaDeviceInfo;
  DeviceName: string;
begin
  OnShow := @FormShow;
  FLastConfig := TUserConfig.Create;
  FChangedConfig := TUserConfig.Create;
  FDefaultConfig := TUserConfig.Create;
  SetControlsAs(GlobalUserConfig);
  pgcOptions.ActivePage := tsTimers;

  { Load audio devices }
  tsAudio.Enabled:=frmMain.AudioWorking;
  if frmMain.AudioWorking then
  begin
    NumDevices := Pa_GetDeviceCount();
    if NumDevices < 0 then
    begin
      DebugLn('Pa_GetDeviceCount failed ');
      DebugLn('Error after Pa_GetDeviceCount ' + IntToStr(NumDevices));
    end;

    DefaultDevice:=Pa_GetDefaultOutputDevice();
    if DefaultDevice = paNoDevice then
    begin
      DebugLn('No default device');
    end;
    DebugLn('Default device is ' + IntToStr(DefaultDevice));

    for Count := 0 to NumDevices - 1 do
    begin
      DeviceInfo := Pa_GetDeviceInfo(Count);
      if DeviceInfo = Nil then
        DebugLn('Error after GetDeviceInfo for device #' + IntToStr(Count))
      else
      begin
        DeviceName:=StrPas(DeviceInfo^.Name);
        //DeviceInfo^.

        if Count = DefaultDevice then
          DeviceName := DeviceName + ' (Default)';
        cmbAudioDevice.Items.Add(DeviceName);
      end;
    end;
  end;
  if cmbAudioDevice.Items.Count > 0 then
  begin
    if GlobalUserConfig.AudioDeviceName = DEF_AUDIO_DEVICE_NAME then
      cmbAudioDevice.ItemIndex:=DefaultDevice
    else
      cmbAudioDevice.ItemIndex:=cmbAudioDevice.Items.IndexOf(GlobalUserConfig.AudioDeviceName);
  end;
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  FLastConfig.Free;
  FChangedConfig.Free;
  FDefaultConfig.Free;
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
  Close;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  SetControlsAs(GlobalUserConfig);
  FLastConfig.CopyFrom(GlobalUserConfig);
end;

end.
