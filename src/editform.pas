unit editform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker;

type
  { TTimerSpecs }
  {TTimerSpecs = class(TObject)
  private
  public
    DurationHours: integer;
    DurationMinutes: integer;
    DurationSeconds: integer;
    //ModalAlert: boolean;
    //TrayNotification: boolean;
    Description: string;
    ModalAlert: boolean;
    TrayNotification: boolean;
    constructor Create();
    destructor Destroy; override;
  end;}

  { TfrmEditTimer }

  TfrmEditTimer = class(TForm)
    bbSelectAudioFile: TBitBtn;
    bbCancel: TBitBtn;
    bbSave: TBitBtn;
    ckbModalAlert: TCheckBox;
    ckbTrayNotification: TCheckBox;
    dtpDuration: TDateTimePicker;
    edtDescription: TEdit;
    edtAudioFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure bbCancelClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FProceed: boolean;
    //FSpecs: TTimerSpecs;
    FDuration: TTime;
    FDescription: string;
    FModalAlert: boolean;
    FTrayNotification: boolean;
    procedure SetDescription(AValue: string);
    procedure SetDuration(AValue: TTime);
    procedure SetFTrayNotification(AValue: boolean);
    procedure SetModalAlert(AValue: boolean);
    function Validate: boolean;
  public
    function ShowAndGetSpecs: boolean;
    function ShowForAdd:boolean;
    function ShowForEdit:boolean;
    //property Specs: TTimerSpecs read FSpecs;
    property Duration: TTime read FDuration write SetDuration;
    property Description: string read FDescription write SetDescription;
    property ModalAlert: boolean read FModalAlert write SetModalAlert;
    property TrayNotification: boolean read FTrayNotification write SetFTrayNotification;
  end;


var
  frmEditTimer: TfrmEditTimer;

implementation

{$R *.lfm}

{ TTimerSpecs }

{procedure TTimerSpecs.SetDurationHours(AValue: integer);
begin
  FDurationHours := AValue;
end;

procedure TTimerSpecs.SetDescription(AValue: string);
begin
  if FDescription='' then Exit;
  FDescription:=AValue;
end;}

{constructor TTimerSpecs.Create();
begin

end;

destructor TTimerSpecs.Destroy;
begin
  inherited Destroy;
end;}

{ TfrmEditTimer }

procedure TfrmEditTimer.FormCreate(Sender: TObject);
begin
  FProceed:=False;

  //FSpecs:=TTimerSpecs.Create();
{  FSpecs.DurationHours:=0;
  FSpecs.DurationMinutes:=10;
  FSpecs.DurationSeconds:=0;
  FSpecs.Description:='Countdown Timer';}
  FDuration:=EncodeTime(0, 10, 0,0);
  FDescription:=' ';
  FModalAlert:=True;
  FTrayNotification:=True;

end;

procedure TfrmEditTimer.bbSaveClick(Sender: TObject);
var
  Hour, Min, Sec, Milli : Word;
begin
  FDescription:=edtDescription.Text;
  {DecodeTime(dtpDuration.Time, Hour, Min, Sec, Milli);
  FSpecs.DurationHours:=Hour;
  FSpecs.DurationMinutes:=Min;
  FSpecs.DurationSeconds:=Sec;}
  FDuration:=dtpDuration.Time;
  FModalAlert:=ckbModalAlert.Checked;
  FTrayNotification:=ckbTrayNotification.Checked;
  FProceed:=True;
  Close;
end;

procedure TfrmEditTimer.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditTimer.FormDestroy(Sender: TObject);
begin
  //FSpecs.Free;
end;

function TfrmEditTimer.Validate: boolean;
begin
  if edtDescription.Text = '' then
  begin
    Result := False;
    Exit;
  end;

end;

procedure TfrmEditTimer.SetDescription(AValue: string);
begin
  if FDescription=AValue then Exit;
  FDescription:=AValue;
  edtDescription.Text:=AValue;
end;

procedure TfrmEditTimer.SetDuration(AValue: TTime);
begin
  if FDuration=AValue then Exit;
  FDuration:=AValue;
  dtpDuration.Time:=AValue;
end;

procedure TfrmEditTimer.SetFTrayNotification(AValue: boolean);
begin
  if FTrayNotification=AValue then Exit;
  FTrayNotification:=AValue;
  ckbTrayNotification.Checked:=AValue;
end;

procedure TfrmEditTimer.SetModalAlert(AValue: boolean);
begin
  if FModalAlert=AValue then Exit;
  FModalAlert:=AValue;
  ckbModalAlert.Checked:=AValue;
end;

function TfrmEditTimer.ShowAndGetSpecs: boolean;
begin
  FProceed:=False;
  {edtDescription.Text:=FSpecs.Description;
  dtpDuration.Time := EncodeTime(FSpecs.DurationHours, FSpecs.DurationMinutes,  FSpecs.DurationSeconds, 0);
  ckbModalAlert.Checked:=FSpecs.ModalAlert;
  ckbTrayNotification.Checked:=Specs.TrayNotification; }
  ShowModal;
  Result:=FProceed;
end;

function TfrmEditTimer.ShowForAdd: boolean;
begin
  Self.Caption:='Add Timer';
  Result:=ShowAndGetSpecs;
end;

function TfrmEditTimer.ShowForEdit: boolean;
begin
  Self.Caption:='Edit Timer';
  Result:=ShowAndGetSpecs;
end;

end.
