{---------------------------------------------------------------------------------
This file is part of OpenARCK Project.

   Copyright (C) 2021  Everton Teles
   email: info@evertonteles.com
   home: www.evertonteles.com

OpenArch is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
----------------------------------------------------------------------------------

// History:
 2021 04 17 - ET - Unit implementation Start's.
}

unit OpenArchMainToolbar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls;

type

  { TArchMainToolBarFrame }

  TArchMainToolBarFrame = class(TFrame)
    ToolBar1: TToolBar;
  private

  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation
uses
OpenArchMainForm;

{$R *.frm}

{ TArchMainToolBarFrame }

constructor TArchMainToolBarFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Parent:= TheOwner as TMainForm;
  Align:= alTop;
end;

end.

