unit editform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker;

type
  { TTimerSpecs }
  TTimerSpecs = class(TObject)
  private
  public
    DurationHours: integer;
    DurationMinutes: integer;
    DurationSeconds: integer;
    Description: string;
    ModalAlert: boolean;
    TrayNotification: boolean;
    constructor Create();
    destructor Destroy; override;
  end;

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
    FSpecs: TTimerSpecs;
    function Validate: boolean;
  public
    function ShowAndGetSpecs: boolean;
    function ShowForAdd:boolean;
    function ShowForEdit:boolean;
    property Specs: TTimerSpecs read FSpecs;
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

constructor TTimerSpecs.Create();
begin

end;

destructor TTimerSpecs.Destroy;
begin
  inherited Destroy;
end;

{ TfrmEditTimer }

procedure TfrmEditTimer.FormCreate(Sender: TObject);
begin
  FProceed:=False;

  FSpecs:=TTimerSpecs.Create();
  FSpecs.DurationHours:=0;
  FSpecs.DurationMinutes:=10;
  FSpecs.DurationSeconds:=0;
  FSpecs.Description:='Countdown Timer';

end;

procedure TfrmEditTimer.bbSaveClick(Sender: TObject);
var
  Hour, Min, Sec, Milli : Word;
begin
  FSpecs.Description:=edtDescription.Text;
  DecodeTime(dtpDuration.Time, Hour, Min, Sec, Milli);
  FSpecs.DurationHours:=Hour;
  FSpecs.DurationMinutes:=Min;
  FSpecs.DurationSeconds:=Sec;
  FProceed:=True;
  Close;
end;

procedure TfrmEditTimer.bbCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEditTimer.FormDestroy(Sender: TObject);
begin
  FSpecs.Free;
end;

function TfrmEditTimer.Validate: boolean;
begin
  if edtDescription.Text = '' then
  begin
    Result := False;
    Exit;
  end;

end;

function TfrmEditTimer.ShowAndGetSpecs: boolean;
begin
  FProceed:=False;
  edtDescription.Text:=FSpecs.Description;
  dtpDuration.Time := EncodeTime(FSpecs.DurationHours, FSpecs.DurationMinutes,  FSpecs.DurationSeconds, 0);
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
