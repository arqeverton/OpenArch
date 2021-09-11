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
 2021 02 10 - ET - Unit implementation Start's.
}

unit OpenArchProperty;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  //OpenArch:


type
  { TArchValue }
  TArchValue = record
    case ValueType:integer of
      0:   (BolValue: Boolean);
      1:   (IntValue: Integer);
      2:   (FltValue: Double);
      3:   (StrValue: String[255]);
      4:   (SetStrValue: set of String[255]);
  end;

  //Forward declaration:
  TArchProperties = class;

  { TArchProperty }

  TArchProperty = class
    private
      FPropertyName,
      FDescription: string;
      FValue: TArchValue;
    public
  end;

  {TArchPropertySet}
  TArchPropertySet = class
    private
      FSetName: string;
      FPropertySet: set of TArchProperty;
    public
  end;

  {TArchProperties}
  TArchProperties = Array of TArchPropertySet;

implementation

end.

