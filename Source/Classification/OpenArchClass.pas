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
unit OpenArchClass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  //OpenArch:
  OpenArchProperty;

type
  TArchClasses = class;

  { TArchClass }
  TArchClass = class
    private
      FID,
      FArchClassName,
      FDescription: string;
      FClasses: TArchClasses;
      FProperties: TArchProperties;
    public

  end;

  TArchClasses = Array of TArchClass;

implementation

end.

