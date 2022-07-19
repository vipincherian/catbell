{

Copyright (C) 2021 Vipin Cherian

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
tsLicense as published by the Free Software Foundation; either
version 2 of the tsLicense, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public tsLicense for more details.

You should have received a copy of the GNU Library General Public
tsLicense along with this library; if not, write to the
Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
Boston, MA  02110-1301, USA.

}

unit aboutform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, LCLIntf, LCLType, FileInfo, util;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    bbClose: TBitBtn;
    imgApp: TImage;
    lblAppName: TLabel;
    lblDescription: TLabel;
    lblAuthor: TLabel;
    lblLicense: TLabel;
    lsvBuild: TListView;
    Panel2: TPanel;
    sbxLicense: TScrollBox;
    pgAdditional: TPageControl;
    pnlApp: TPanel;
    sttUrl: TStaticText;
    tsLicense: TTabSheet;
    tsBuild: TTabSheet;
    procedure bbCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure sttUrlClick(Sender: TObject);
  private
    procedure LayoutControls;
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }


procedure TfrmAbout.bbCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  Stream: TResourceStream;
  LicenseText: string;
  Buffer: Pointer;
  Item: TListItem;
  Info: TFileVersionInfo;
begin
  { Read file version information and add }
  Info := TFileVersionInfo.Create(Self);
  Info.ReadFileInfo;
  lblAppName.Caption := Info.VersionStrings.Values['ProductName'] +
    ' ' + Info.VersionStrings.Values['FileVersion'];
  Info.Free;

  Stream := TResourceStream.Create(hinstance, 'GPL2', RT_RCDATA);
  Buffer := AllocMem(Stream.Size + 1);
  Stream.ReadBuffer(Buffer^, Stream.Size);
  LicenseText := pansichar(Buffer);
  lblLicense.Caption := LicenseText;
  Freemem(Buffer);
  Stream.Free;
  lsvBuild.BeginUpdate;

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Target OS';
  Item.SubItems.Add(
    {$include %FPCTargetOS%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Target CPU';
  Item.SubItems.Add(
    {$include %FPCTargetCPU%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Compiler(FPC) Version';
  Item.SubItems.Add(
    {$include %FPCVersion%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Build date';
  Item.SubItems.Add(
    {$include %DATE%}
    );

  Item := lsvBuild.Items.Add;
  Item.Caption := 'Build time';
  Item.SubItems.Add(
    {$include %TIME%}
    );

  lsvBuild.EndUpdate;

  pgAdditional.ActivePage := tsLicense;

  LayoutControls;

end;


procedure TfrmAbout.sttUrlClick(Sender: TObject);
begin
  OpenUrl(sttUrl.Caption);
end;

procedure TfrmAbout.LayoutControls;
begin
  { Dynamic layout of controls }

  with UserInterfaceMetrics do
  begin

    { Controls on which the rest are anchored }
    bbClose.BorderSpacing.Bottom := Margin;
    bbClose.BorderSpacing.Right := Margin;

    with pnlApp.BorderSpacing do
    begin
      Top := Margin;
      Right := Margin;
      Left := Padding;
    end;

    { Rest of the dependent controls }

    imgApp.BorderSpacing.Left := Margin;

    lblAppName.BorderSpacing.Left := Padding;
    lblAppName.BorderSpacing.Top := Padding;
    lblAuthor.BorderSpacing.Top := Padding;
    lblDescription.BorderSpacing.Top := 2 * Margin;
    sttUrl.BorderSpacing.Top := Padding;

    pnlApp.BorderSpacing.Top := Padding;

    pgAdditional.BorderSpacing.Top := Padding;
    pgAdditional.BorderSpacing.Bottom := Padding;

    with sbxLicense.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
      Right := Padding;
      Bottom := Padding;
    end;

    with lsvBuild.BorderSpacing do
    begin
      Top := Padding;
      Left := Padding;
      Right := Padding;
      Bottom := Padding;
    end;

  end;
end;

end.
