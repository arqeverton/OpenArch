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
 2021 02 11 - ET - Unit implementation Start's.
}
unit OpenArchIOFileA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  { GLScene }
  
  { Openarch }
  OpenArchVectorGeometry,
  OpenArchSysParameters,
  OpenarchEnumTypes;
  
type
  { IArchReader }
  IArchReader = interface 
    procedure ReadParameters(var Param,Value: String);
    //Lê um ponto 3D
    function Read3DPoint(const Value:String): IArch3DPoint;
  end;
  
  { IArchWriter }
  IArchWriter = interface
    //Escrever parametros vindos do sistema no arquivo
    procedure WriteParam(Param,Value: String);
    procedure Write3DPoint(A3DPoint: IArch3DPoint; ABaseParam: String);
    procedure WriteSection(ASection:String);
    Procedure WriteObject(AId:String);
    Procedure WriteEndObject();
  end;
  
  IArchIOFile = interface
    procedure SaveData(AParams: TArchParamList);
    function LoadData():TArchParamList;
  end;
  

implementation

end.

