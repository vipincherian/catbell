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
    edDescription: TEdit;
    edtAudioFile: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public

  end;

var
  frmEditTimer: TfrmEditTimer;

implementation

{$R *.lfm}

{ TfrmEditTimer }

procedure TfrmEditTimer.FormCreate(Sender: TObject);
begin

end;

procedure TfrmEditTimer.FormDestroy(Sender: TObject);
begin

end;

end.

