unit editform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  DateTimePicker;

type

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    function Validate: boolean;
  public

  end;

  { TTimerSpecs }
  TTimerSpecs = class(TObject)
  private
    FDescription: string;
    FDuration: integer;
    procedure SetDuration(AValue: integer);
  public
    ModalAlert: boolean;
    TrayNotification: boolean;
    constructor Create();
    destructor Destroy; override;
    property Duration: integer read FDuration write SetDuration;
  end;

var
  frmEditTimer: TfrmEditTimer;

implementation

{$R *.lfm}

{ TTimerSpecs }

procedure TTimerSpecs.SetDuration(AValue: integer);
begin
  FDuration := AValue;
end;

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

end;

procedure TfrmEditTimer.FormDestroy(Sender: TObject);
begin

end;

function TfrmEditTimer.Validate: boolean;
begin
  if edtDescription.Text = '' then
  begin
    Result := False;
    Exit;
  end;

end;

end.
