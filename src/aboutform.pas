{

Copyright (C) 2017 Vipin Cherian

This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

}

unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Image1: TImage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure FormDestroy(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Image1Click(Sender: TObject);
begin

end;

procedure TfrmAbout.FormDestroy(Sender: TObject);
begin

end;

procedure TfrmAbout.StaticText1Click(Sender: TObject);
begin

end;

procedure TfrmAbout.StaticText3Click(Sender: TObject);
begin

end;

end.

