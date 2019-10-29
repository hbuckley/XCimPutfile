CREATE OR REPLACE package x_cim_putfile
as
  --
  g_sccsid      varchar2(100);
  g_body_sccsid varchar2(100);
  --
  FUNCTION get_version RETURN varchar2;
  --
  FUNCTION get_body_version RETURN varchar2;
  --
  Function f$IsTermContract( pi_wor in work_orders.wor_works_order_no%type
                 ,       pi_contr_code in org_units.oun_contractor_id%type default 'RWY')
  return boolean;
  --
  Function f$GetBoqValue(pi_works_order_no in work_orders.wor_works_order_no%type
                    ,    pi_boq_id         in boq_items.boq_sta_item_code%type )
  return boq_items%rowtype;
  --
  Procedure show(msg in varchar2);
  --
  Procedure GetFcFile;
  --
  FUNCTION Is_Open(file_in IN utl_file.file_type)
  RETURN BOOLEAN;
  --
  FUNCTION Writeln(File_Handle IN utl_file.File_Type
                 , line_in     IN VARCHAR2 )
  RETURN BOOLEAN;
  --
  PROCEDURE  Create_File(loc_in  IN hig_directories.hdir_path%type
                        ,file_in IN VARCHAR2
                        ,line_in IN VARCHAR2);
  --
  FUNCTION FileExists( loc_in   IN VARCHAR2
                      ,file_in  IN VARCHAR2 )
  RETURN BOOLEAN;
  --
  PROCEDURE write_file(pi_file    in varchar2 default 'clive.dat'
                      ,pi_dir     in hig_directories.hdir_name%type default 'CIM_DIR'
					  ,pi_payment_date in date
					  ,pi_debug   in char default 'N');
  --
end x_cim_putfile;
/


CREATE OR REPLACE PACKAGE BODY x_cim_putfile AS
--
-----------------------------------------------------------------------------
--
--   PVCS Identifiers :-
--
--       pvcsid           : $Header  :   x_cim_putfile.pkb $
--       Module Name      : $Workfile:   x_cim_putfile.pkb $
--       Date into PVCS   : $Date    :   18 Apr 2019       $
--       Date fetched Out : $Modtime :   18 Apr 2019       $
--       PVCS Version     : $Revision:   1.0               $
--
--       Date Changed     : 24/05/2017
--
-----------------------------------------------------------------------------
--  Copyright (c) exor corporation ltd, 2007
-----------------------------------------------------------------------------
-- **************************************************************************
-- This package has been specifically written for Worcestershire 4700 system.
-- **************************************************************************
-- This package is going to allow the user to:
--
--   (a) Create a pl/sql collection
--   (b) Populate the collection with data
--   (c) Dump the collection to a specified file to a specified location ( Oracle Directory ).
--
-- Usage : x_cim_putfile.write_file(<File>,<Directory>,<Payment Date>,<Debug>)
--
-- Labour : Labour is contained within a BOQ item.
--          At the moment we believe that its in the '100' item and that there
--          may be more then 1 item per work order line. There may be a change to the
--          system with the introduction of a '102' item.
--          The current calculation will theerfore SUM all 100 values and ADD any 102 values.
--
-- Parameters:
--
--  The E5 extract will be driven by a payment date.
--
--
-- ###################
-- Constants
-- ###################
g_package_name           CONSTANT varchar2(30)    := 'x_cim_putfile.pkb';
--
gt_tokens  nm3type.tab_varchar32767;

-- ###################
-- Globals
-- ###################
g_directory_name                hig_directories.hdir_name%TYPE:='CIM_DIR';
g_gl_code                       budgets.bud_cost_code%type:='64951';
g_lab1_boq                      boq_items.boq_sta_item_code%type :='100';
g_lab2_boq                      boq_items.boq_sta_item_code%type :='102';
g_vat_boq                       boq_items.boq_sta_item_code%type :='VAT20';
--
--
-- ###################
-- Exceptions
-- ###################
invalid_file            EXCEPTION;
invalid_record          EXCEPTION;
unable_to_write_to_file EXCEPTION;
unable_to_close_file    EXCEPTION;
--
--
-- ##################
-- Record Structures
-- ##################
--
-- We need to check the following structure against the
-- document provided by Lee.
-- no,name,structurekey,easting,northing,kilometerage,structuretype,road
--
--
TYPE outrec IS RECORD
( output_rec varchar2(500));
--
type outrec_tab is table of outrec index by binary_integer;
--
-- ##################
-- Variables
-- ##################
--
asd_load_directory hig_directories.hdir_name%type;
outrec_file        outrec_tab;
period13           hig_option_values.hov_value%type:=hig.get_sysopt('PERIOD13');
--
--
File_Handle        utl_file.file_type;  -- File handle
Output_File        utl_file.file_type;  -- File handle
FRead              CHAR:='R';           -- Read Character
FWrite             CHAR:='W';           -- Write character
--
dbug               boolean:=false;
--
-----------------------------------------------------------------------------
--
-- ################################################
-- ################ FUNCTIONS   ###################
-- ################################################
--
FUNCTION get_version RETURN varchar2 IS
BEGIN
   RETURN g_sccsid;
END get_version;
--
-----------------------------------------------------------------------------
--
FUNCTION get_body_version RETURN varchar2 IS
BEGIN
   RETURN g_body_sccsid;
END get_body_version;
--
-----------------------------------------------------------------------------
--
--
-- This function is to be used to define a work order that is either TERM Contract
-- or NON Term contract. A Term contract is one whee the contractor id is 'RWY'
--
Function f$IsTermContract( pi_wor        in work_orders.wor_works_order_no%type
                 ,         pi_contr_code in org_units.oun_contractor_id%type default 'RWY')
return boolean
is
   cursor c1
   is select 'Y'
      from   contracts
        ,    work_orders
		,    org_units
	  where  wor_works_order_no = pi_wor
      and    wor_con_id         = con_id
      and    con_contr_org_id   = oun_org_id
	  and    oun_org_unit_type  = 'CO'
	  and    oun_contractor_id  = pi_contr_code;
	vOk char;
begin
  open c1;
  fetch c1 into vOk;
  if c1%found
  then close c1;
       return true;
  else close c1;
       return false;
  end if;
end f$IsTermContract;
--
-- Some BOQ items are used to maintain certain field values and we need
-- a function that can take paramrters to return the values.
--
Function f$GetBoqValue(pi_works_order_no in work_orders.wor_works_order_no%type
                    ,    pi_boq_id         in boq_items.boq_sta_item_code%type )
return boq_items%rowtype
is
     cursor c1
	 is select a.*
	    from  boq_items a
		   ,  work_order_lines b
		where boq_wol_id         = wol_id
		and   boq_sta_item_code  = pi_boq_id
        and   wol_works_order_no = pi_works_order_no
        and   rownum=1;

	 boqRec boq_items%rowtype;

  begin
     open  c1;
	 fetch c1 into boqRec;
	 close c1;
	 return boqRec;
  end f$GetBoqValue;
--
-- #################################################
-- ########## PROCEDURES ###########################
-- #################################################
--
procedure show(msg in varchar2)
is
  begin
     dbms_output.put_line(msg);
  end;
--
-----------------------------------------------------------------------------
--
-- #################################################
-- ########## FILE FUNCTIONS #######################
-- #################################################
--
-----------------------------------------------------------------------------
--
-- *****************************************************************************
-- Closes an open file.
-- *****************************************************************************
FUNCTION Close_File(file_in IN utl_file.file_type)
RETURN BOOLEAN
IS
BEGIN
  utl_file.fclose(File_Handle);
  IF is_open(File_Handle)
  THEN RETURN FALSE;
  ELSE RETURN TRUE;
  END IF;
END;
--
-----------------------------------------------------------------------------
--
-- *****************************************************************************
-- Check to see wether a specified file is open
-- *****************************************************************************
FUNCTION Is_Open(file_in IN utl_file.file_type)
RETURN BOOLEAN
IS
BEGIN
  IF utl_file.is_open(file_in)
  THEN RETURN TRUE;
  ELSE RETURN FALSE;
  END IF;
END;
--
-- *****************************************************************************
-- Write an output line to a file
-- *****************************************************************************
--
FUNCTION Writeln(File_Handle IN utl_file.File_Type, line_in IN VARCHAR2 )
RETURN BOOLEAN
IS
BEGIN
  --
  utl_file.put_line(File_Handle,line_in);
  utl_file.fflush(File_Handle);
  --
  RETURN TRUE;
EXCEPTION
  WHEN OTHERS
  THEN RETURN FALSE;
END;
--
-----------------------------------------------------------------------------
--
-- *****************************************************************************
-- Procedure Create File
-- *****************************************************************************
--
-----------------------------------------------------------------------------
--
PROCEDURE  Create_File(loc_in  IN hig_directories.hdir_path%type
                      ,file_in IN VARCHAR2
                      ,line_in IN VARCHAR2)
IS
procedure show(msg in varchar2)
is
  begin
     dbms_output.put_line(msg);
  end;
BEGIN
  -- Open the specified file for writing
  IF dbug
  THEN show('{ Function - Create File }');
       show('Location : '||loc_in);
       show('File     : '||file_in);
  END IF;
  -- Here we are going to set a max line length of 262 charaters
  -- the default is 1024 and we can go to 32767 if necessary
  file_handle:=utl_file.fopen(loc_in,file_in,FWrite,500);
  IF   line_in IS NOT NULL
  THEN utl_file.put_line(file_handle,line_in);
  ELSE utl_file.put_line(file_handle,'Info: End-Of-File here');
  END IF;
  --
EXCEPTION
  WHEN OTHERS
  THEN dbms_output.put_line('Error: An unknown error occured - {Create_File}');
  RAISE;
END;
--
-----------------------------------------------------------------------------
--
-- *****************************************************************************
-- Check for the existance of a specified file
-- *****************************************************************************
FUNCTION FileExists( loc_in   IN VARCHAR2
                    ,file_in  IN VARCHAR2 )
RETURN BOOLEAN
IS
procedure show(msg in varchar2)
is
  begin
     dbms_output.put_line(msg);
  end;
BEGIN
  -- Open the file
  IF dbug
  THEN show('Path   : '||loc_in);
       show('File   : '||file_in);
  END IF;
  --
  IF NOT is_open(file_handle)
  THEN IF NOT Close_File(File_handle)
       THEN show('Info: Unable to close open file.');
       END IF;
    file_handle:=utl_file.fopen(loc_in,file_in,FRead,32767);
  END IF;
  --
  IF dbug
  THEN show('Info: Handle Obtained - {FileExists}');
  END IF;
  --
  -- Return the result of a check with IS_OPEN
  --
  utl_file.fclose(file_handle);
  --
  RETURN TRUE;
  --
EXCEPTION
  WHEN OTHERS
  THEN RETURN FALSE;
END;
--
-----------------------------------------------------------------------------
--
FUNCTION write_the_file(pi_filename         IN varchar2
                       ,pi_oracle_directory IN hig_directories.hdir_name%TYPE:='CIM_DIR'
                       )
return boolean
is

   ok             boolean:=true;
   FRead          CHAR:='R';           -- Read Character
   FWrite         CHAR:='W';           -- Write character

procedure show(msg in varchar2)
is
  begin
     dbms_output.put_line(msg);
  end;
--
-- Obtain the path of the specified input diretory
--
Function f$GetDirPath( pi_dir in hig_directories.hdir_name%type)
return varchar2
is
    cursor c1
	is select hdir_path
	   from   hig_directories
	   where  hdir_name=pi_dir;
	--
	po_dir hig_directories.hdir_path%type;
	--
begin
   open  c1;
   fetch c1 into po_dir;
   if c1%notfound
   then po_dir:='Unknown';
   end if;
   close c1;
   return po_dir;
end f$GetDirPath;

begin
   if outrec_file.count>0
   then create_file(pi_oracle_directory,pi_filename,outrec_file(1).output_rec);
        for j in 2..outrec_file.count
        loop if (length(outrec_file(j).output_rec)>0
		     and substr(outrec_file(j).output_rec,1,1)in ('A','B','C'))
			 then if NOT writeln(File_Handle,outrec_file(j).output_rec)
				  then raise unable_to_write_to_file;
				       ok:=false;
				  end if;
             end if;
		end loop;
   --
   if NOT Close_File(File_handle)
   then raise unable_to_close_file;
        ok:=false;
   end if;
   else show('Error: There was no data selected to output to a file');
   end if;
   --
   return ok;
   exception
   when unable_to_write_to_file
   then hig_process_api.log_it(pi_message => 'Error{output_smis_data}: Unable to write to the SMIS loader output file.');
        return false;
   when unable_to_close_file
   then hig_process_api.log_it(pi_message => 'Error{output_smis_data}: Unable to close the SMIS loader output file.');
        return false;
   when others
   then hig_process_api.log_it(pi_message => 'Error{output_smis_data}: An unknown error occured while creating the output file.');
   return false;
   --
end write_the_file;
--
-----------------------------------------------------------------------------
--
-- We are going to output the FC file to the same format as defined.
--
Procedure GetFcFile
is
begin
  null;
end;
--
-----------------------------------------------------------------------------
--
-- 1. Populate the header
-- 2. Populate the body
-- 3. Populate the footer
-- 4. Output the data to a file
--
-----------------------------------------------------------------------------
PROCEDURE write_file(pi_file    in varchar2 default 'clive.dat'
                    ,pi_dir     in hig_directories.hdir_name%type default 'CIM_DIR'
					,pi_payment_date in date
					,pi_debug   in char default 'N')
IS
--

  cursor GetRelatedWorkOrders(pi_payment_date in date )
  is select distinct wol_works_order_no
                    ,cp_payment_id
     from  work_order_lines
     ,     claim_payments
     where cp_wol_id=wol_id
     and   trunc(cp_payment_date)=pi_payment_date
	 --and wol_works_order_no='BRI/2936'
	 --and wol_works_order_no='WCC/32'
     order by cp_payment_id,wol_works_order_no;

  cursor GetRelatedPayments(pi_payment_date in date )
  is select a.*
           ,b.wol_works_order_no work_order_no
     from   claim_payments     a
	       ,work_order_lines   b
	 where  trunc(a.cp_payment_date)=pi_payment_date
	 and    b.wol_id=a.cp_wol_id
	 order by a.cp_payment_id;

  -- This cursor should be used to construct the work order header record ( A Record )
  Cursor WorkOrderHeader(pi_payment_id     in claim_payments.cp_payment_id%type
                       , pi_works_order_no in work_order_lines.wol_works_order_no%type
				       , pi_vna_val        in varchar2
					   , IsWoTmc           in boolean)
  is
   SELECT DISTINCT
	('A'
    ||-- Vendor Number ( Contractor Code )
	  case xworc_is_number(o.oun_unit_code)
      when 1
	  then lpad(o.oun_unit_code,10,0)
      else rpad(o.oun_unit_code,10,' ')
      end
    ||-- Vendor Name
	  rpad(o.oun_name,267)
    ||-- Financial Year
	  to_char(decode(hig.get_sysopt('PERIOD13'),'Y',fy.fyr_id-1,fy.fyr_id))
    ||lpad(nvl(o.oun_credit_type,'  '),2,' ')
    ||nvl(hig.get_sysopt('PERIOD13'),'N')
	||rpad(substr(pi_vna_val,1,16),16,' ')
    ||rpad(substr(wor.wor_works_order_no
    ||'-'
    ||to_char(b.cp_payment_id)
    ||'-'
    ||wor.wor_descr,
          1,25),25)
    ||lpad(to_char(trunc(b.cp_payment_date),'RRRRMMDD'),43,' ')
    ||lpad('LA01',24)
	||'  '
     ) HeaderRecord
FROM  claim_payments            b
    , org_units                 o
    , financial_years          fy
	, work_order_lines         wol
	, work_orders              wor
	, contracts                con
WHERE  b.cp_payment_id           = pi_payment_id
   and wor.wor_works_order_no    = pi_works_order_no
   and b.cp_wol_id               = wol.wol_id
   and con.con_id                = b.cp_woc_con_id
   and con.con_contr_org_id      = o.oun_org_id
   and trunc(b.cp_payment_date) between fy.fyr_start_date
                                and     fy.fyr_end_date
   and wor.wor_works_order_no    = wol.wol_works_order_no
   and nvl(b.cp_payment_value,0) <> 0;

-- *************
-- Variables
-- *************
vno_val              varchar2(200);
RecLine              varchar2(500):='C';
--
BudRec               budgets%rowtype;
ConRec               contracts%rowtype;
PayRec               claim_payments%rowtype;
--
pi_bud_cost_code     budgets.bud_cost_code%type;
pi_gl_code           varchar2(50) :='';
pi_tax_code          varchar2(50) :='';
pi_internal_order_no varchar2(50) :='';
Brec_Net             number:=0;
pi_line_text         varchar2(200):='';
pi_net_amount        number:=0;
pi_vat_amount        number:=0;
Brec_Vat             number:=0;
Brec_Labour          number:=0;
recno                integer:=1;
--
Function f$GetDirPath( pi_dir in hig_directories.hdir_name%type)
return varchar2
is
    cursor c1
	is select hdir_path
	   from   hig_directories
	   where  hdir_name=pi_dir;
	--
	po_dir hig_directories.hdir_path%type;
	--
begin
   open  c1;
   fetch c1 into po_dir;
   if c1%notfound
   then po_dir:='Unknown';
   end if;
   close c1;
   return po_dir;
end f$GetDirPath;
--
Function f$SetBoolVal( piVal in char)
return boolean
is
  ok boolean:=false;
begin
   if upper(piVal)='Y'
   then ok:=true;
   end if;
   return ok;
end f$SetBoolVal;
--
Function f$GetWorksOrderNo( pi_cp_wol_id in claim_payments.cp_wol_id%type)
return work_orders.wor_works_order_no%type
is
    cursor c1
	is select wol_works_order_no
	   from   work_order_Lines
	   where  wol_id=pi_cp_wol_id;
	c1Rec c1%rowtype;
begin
  open c1;
  fetch c1 into c1Rec;
  close c1;
  return c1Rec.wol_works_order_no;
end f$GetWorksOrderNo;
--

Function f$GetVnoVal(pi_works_order_no in work_orders.wor_works_order_no%type
             ,       pi_payment_id     in claim_payments.cp_payment_id%type
			 ,       pi_wol_id         in work_order_Lines.wol_id%type
             ,       pi_boq_id         in boq_items.boq_sta_item_code%type default 'VNO'
			 ,       pi_tmc            in boolean default false)
return varchar2
is
    cursor c1
	 is select a.*
	          ,b.wol_id
	    from  boq_items        a
		   ,  work_order_lines b
		   ,  claim_payments   c
		where a.boq_wol_id         = b.wol_id
		and   a.boq_sta_item_code  = pi_boq_id
        and   b.wol_works_order_no = pi_works_order_no
		and   c.cp_wol_id          = b.wol_id
		and   c.cp_wol_id          = pi_wol_id;

    cursor c2( pi_works_order in work_orders.wor_works_order_no%type)
    is select hau_authority_code
       , 	  hau_unit_code
	   from hig_admin_units
	      , work_orders
		  , contracts
		  , org_units
	   where wor_works_order_no    = pi_works_order_no
	   and   wor_con_id            = con_id
       and   con_contr_org_id      = oun_org_id
       and   oun_admin_org_id      = hau_admin_unit;
       --
	cursor c3
	is select rpad(substr(con_external_ref,1,16),16) con_external_ref
	   from   contracts
	       ,  work_orders
	   where  wor_works_order_no=pi_works_order_no
	   and    wor_con_id        =con_id;

	   c1Rec       c1%rowtype;
	   c2Rec       c2%rowtype;
	   tmp_vno_val varchar2(200);
	   vno_val     varchar2(200);
	   --
  begin
    if not pi_tmc -- Term Maintenance Contract related work order
	then open  c1;
	     fetch c1 into c1Rec;
	     if c1%notfound
	     then close c1;
	     else if c1Rec.boq_item_name is null
       	      then open  c2(pi_works_order_no);
		           fetch c2 into c2Rec;
			       close c2;
		           vno_val:=rpad(substr(nvl(c2Rec.hau_authority_code,'000'),1,3)||substr(c2Rec.hau_unit_code,1,3) || pi_works_order_no || '-' || to_char(pi_payment_id),16);
		      else vno_val:=rpad(substr(c1Rec.boq_item_name,1,16),16);
		      end if;
		 end if;
    else vno_val:=rpad(pi_works_order_no||'-'||to_char(pi_payment_id),17,' ');
	end if;
	return vno_val;
  end;

  Function f$GetLineText(pi_works_order_no in work_orders.wor_works_order_no%type
                 ,       pi_wol_id         in work_order_lines.wol_id%type
                 ,       pi_payment_id     in claim_payments.cp_payment_id%type
                 ,       pi_boq_id         in boq_items.boq_sta_item_code%type default 'VNO')
  return varchar2
  is
    cursor c1
	 is select substr(a.boq_item_name,1,length(a.boq_item_name)) boq_item
	    from  boq_items        a
		   ,  work_order_lines b
		where boq_wol_id         = wol_id
		and   boq_sta_item_code  = pi_boq_id
        and   wol_works_order_no = pi_works_order_no
		and   wol_id             = pi_wol_id
        and   rownum=1;

	   c1Rec    c1%rowtype;
       LineText varchar2(200);
	   --
  begin
     open  c1;
	 fetch c1 into c1Rec;
	 if c1%found
	 then close c1;
	      LineText:=pi_works_order_no||'-'||to_char(pi_wol_id)||'-'||c1Rec.boq_item;
     else close c1;
          LineText:= pi_works_order_no
		       ||'-'
			   ||to_char(pi_wol_id)
			   ||'-'
			   ||pi_works_order_no
		       ||'-'
               ||to_char(pi_payment_id);
     end if;
  -- This function returns the full budget record for the specifed work order line.
	return LineText;
  end f$GetLineText;

  Function f$GetBudRec( pi_bud_id in budgets.bud_id%type)
  return budgets%rowtype
  is
       cursor c1
	   is select *
	       from  budgets
		   where bud_id = pi_bud_id;
	   BudRec budgets%rowtype;
    begin
	  open c1;
	  fetch c1 into BudRec;
	  close c1;
	  return BudRec;
	end f$GetBudRec;

  -- This function returns the full contract record for the specifed work order line.
  Function f$GetConRec( pi_wol_id in work_order_lines.wol_id%type)
  return contracts%rowtype
  is
       cursor c1
	   is select a.*
	       from  contracts    a
		      ,  work_orders  b
			  ,  work_order_lines c
		   where c.wol_id             = pi_wol_id
		   and   c.wol_works_order_no = b.wor_works_order_no
		   and   b.wor_con_id         = a.con_id;

	   ConRec contracts%rowtype;
    begin
	  open  c1;
	  fetch c1 into ConRec;
	  close c1;
	  return ConRec;
	end f$GetConRec;

  -- This function returns the full contract record for the specifed work order line.
  Function f$GetPayRec( pi_payment_id in claim_payments.cp_payment_id%type
                    ,   pi_wol_id     in work_order_Lines.wol_id%type)
  return claim_payments%rowtype
  is
       cursor c1
	   is select *
	       from  claim_payments
		   where cp_payment_id = pi_payment_id
		   and   cp_wol_id     = pi_wol_id;
	   --
	   PayRec claim_payments%rowtype;
	   --
    begin
	  open  c1;
	  fetch c1 into PayRec;
	  close c1;
	  return PayRec;
	end f$GetPayRec;

  Function f$GetLabourEst( pi_wol_id in work_order_lines.wol_id%type)
  return number
  is
     cursor sum_est_labour
	 is select sum(boq_est_quantity)
        from  boq_items
	    where boq_wol_id       = pi_wol_id
	    and   boq_sta_item_code=g_lab1_boq;

     cursor est_labour
	 is select sum(boq_est_quantity)
        from  boq_items
	    where boq_wol_id       = pi_wol_id
	    and   boq_sta_item_code=g_lab2_boq;

    SumTot number:=0;
    EstTot number:=0;
	LabTot number:=0;

  begin
     open  sum_est_labour;
	 fetch sum_est_labour into SumTot;
	 close sum_est_labour;

     open  est_labour;
	 fetch est_labour into EstTot;
	 close est_labour;

     return nvl(SumTot+EstTot,0);

  end f$GetLabourEst;

  Function f$GetLabourAct( pi_wol_id in work_order_lines.wol_id%type)
  return number
  is
     cursor sum_act_labour
	    and   boq_sta_item_code= '102';
	 is select sum(boq_act_quantity)
        from  boq_items
	    where boq_wol_id       = pi_wol_id

     cursor act_labour
	 is select sum(boq_act_quantity)
        from  boq_items
	    where boq_wol_id       = pi_wol_id
	    and   boq_sta_item_code= g_lab2_boq;

    SumTot number:=0;
    ActTot number:=0;
	LabTot number:=0;

  begin
     open  sum_act_labour;
	 fetch sum_act_labour into SumTot;
	 close sum_act_labour;
	 /*
     open  act_labour;
	 fetch act_labour into ActTot;
	 close act_labour;
    */
     return nvl(SumTot,0);
  end f$GetLabourAct;

  Function f$GetVatValue( pi_wol_id in work_order_lines.wol_id%type)
  return number
  is

     cursor get_act_vat(pi_vat_boq in boq_items.boq_sta_item_code%type)
	 is select sum(boq_act_quantity) boq_act_quantity
        from  boq_items
	    where boq_wol_id       = pi_wol_id
	    and   boq_sta_item_code= pi_vat_boq;

    ActVat number :=0;
	Vat20  number :=0;
	Vat17  number :=0;
	Vat5   number :=0;
	TaxCode varchar2(2):='VO';

Function f$VatExists(pi_wol_id in work_order_lines.wol_id%type)
return boolean
is
   cursor c1
   is select 1
      from  boq_items
      where boq_wol_id = pi_wol_id
      and   boq_sta_item_code in ('VAT20','VAT17','VAT5');
   BoqTot  integer;
   vOk     boolean:=false;

begin
   open  c1;
   fetch c1 into BoqTot;
   vOk:=c1%found;
   close c1;
   return vOk;
end f$VatExists;

  begin
     if f$VatExists(pi_wol_id)
	 then open  get_act_vat('VAT20');
	      fetch get_act_vat into Vat20;
	      close get_act_vat;

		  open  get_act_vat('VAT17');
	      fetch get_act_vat into Vat17;
	      close get_act_vat;

		  open  get_act_vat('VAT5');
	      fetch get_act_vat into Vat5;
	      close get_act_vat;
          --
          return(nvl(vat20,0)+nvl(vat17,0)+nvl(vat5,0));
	 else return 0;
	 end if;
  end f$GetVatValue;

  Function f$GetVatTaxCode( pi_wol_id in work_order_lines.wol_id%type)
  return varchar2
  is

     cursor get_act_vat(pi_vat_boq in boq_items.boq_sta_item_code%type)
	 is select sum(boq_act_quantity) boq_act_quantity
        from  boq_items
	    where boq_wol_id       = pi_wol_id
	    and   boq_sta_item_code= pi_vat_boq;

    ActVat number :=0;
	Vat20  number :=0;
	Vat17  number :=0;
	Vat5   number :=0;
	TaxCode varchar2(2):='VO';

Function f$VatExists(pi_wol_id in work_order_lines.wol_id%type)
return boolean
is
   cursor c1
   is select 1
      from  boq_items
      where boq_wol_id = pi_wol_id
      and   boq_sta_item_code in ('VAT20','VAT17','VAT5');
   BoqTot  integer;
   vOk     boolean:=false;

begin
   open  c1;
   fetch c1 into BoqTot;
   vOk:=c1%found;
   close c1;
   return vOk;
end f$VatExists;

  begin
     if f$VatExists(pi_wol_id)
	 then open  get_act_vat('VAT20');
	      fetch get_act_vat into Vat20;
	      close get_act_vat;

		  open  get_act_vat('VAT17');
		  open  get_act_vat('VAT5');
	      fetch get_act_vat into Vat17;
	      close get_act_vat;

	      fetch get_act_vat into Vat5;
	      close get_act_vat;

		  If    Vat20>0 then TaxCode:='VR';
		  elsif Vat17>0 then TaxCode:='VS';
		  elsif Vat5>0  then TaxCode:='VL';
		  else TaxCode:='VO';
		  end if;
          --
     else TaxCode:='VO';
	 end if;
	 return nvl(TaxCode,'VO');
  end f$GetVatTaxCode;

  Function f$Spaces( pi_spaces in number )
  return varchar2
  is
      outline varchar2(100):='                                                                                                  ';
     begin
	    return substr(outline,1,pi_spaces);
     end f$Spaces;

  Function f$Replace(pi_val1 in varchar2
                   , pi_val2 in varchar2
                   , pi_val3 in varchar2)
  return varchar2
  is
       cursor c1
	   is select replace(pi_val1,pi_val2,pi_val3)
	      from dual;
		po_val varchar2(500);
     begin
	    open  c1;
		fetch c1 into po_val;
		close c1;
		return po_val;
	 end f$Replace;
begin
   --
   -- Here we are going to extract the FC file.
   -- We are then going to extract the file that we really require.
   --
   dbms_output.enable(buffer_size => NULL);
   show('Starting ....');
   dbug:=f$SetBoolVal(pi_debug);
   --
   if dbug
   then show('Info: DEBUGGING enabled.');
        show('File : '||pi_file);
		show('Dir  : '||pi_dir);
		show('Date : '||to_char(pi_payment_date,'DD-MON-YYYY'));
		show('Dbg  : '||pi_debug);
   end if;
   --
   if f$GetDirPath(pi_dir)='Unknown'
   then show('Error: There is a problem with the '||pi_dir||' definition. Check DIRECTORIES');
   else show('File: '||pi_file||' Dir: '||pi_dir);
        --
		for i in GetRelatedWorkOrders(pi_payment_date)
		loop if dbug
		     then show('Work Order: '||i.wol_works_order_no||' Payment Id : '||to_char(i.cp_payment_id));
			 end if;
		     -- So having obtained the work order we need to construct the related work order 'A' record.
			 -- We can obtain the BOQ related values for any of the BOQ's associated to the related Work Order Lines
			 --
			      if f$IsTermContract(i.wol_works_order_no,'RWY')
				  then show('Contract for work order '||i.wol_works_order_no||' is term contract.');
				  else show('Contract for work order '||i.wol_works_order_no||' is NOT term contract.');
				  end if;

				  for j in WorkOrderHeader(i.cp_payment_id
                                         , i.wol_works_order_no
							             , '@@@@@@@@@@@@@@@@' -- vno_val
										 , IsWoTmc => f$IsTermContract(i.wol_works_order_no,'RWY'))
				  loop
					 -- Work Order Lines. Here we are going to construct the work order lines. We are going
					 -- to obtain each calculated value and then concatenate the value into the wol record string.
					 recno      :=1;
					 Brec_Net   :=0;
                     Brec_Vat   :=0;
                     Brec_Labour:=null;
					 for k in ( select a.* ,rownum
					            from   work_order_lines a
								where  a.wol_works_order_no=i.wol_works_order_no
								and exists ( select null
								             from   claim_payments b
											 where  cp_wol_id      = a.wol_id
                                             and    b.cp_payment_id= i.cp_payment_id )
								             order by a.wol_id )
					 loop vno_val:=f$GetVnoVal(i.wol_works_order_no
				                              ,i.cp_payment_id
											  ,k.wol_id
									          ,'VNO'
									          ,f$IsTermContract(i.wol_works_order_no,'RWY'));
                       	  --show('WOL : '||to_char(k.wol_id)||' VNO VAL : '||vno_val||'  Length: '||to_char(length(vno_val)));
            			  --show(j.headerrecord);
						  j.HeaderRecord:=f$replace(j.HeaderRecord,'@@@@@@@@@@@@@@@@',rpad(vno_val,16,' '));
						  --show(j.headerrecord);
					      if not f$IsTermContract(i.wol_works_order_no,'RWY') or recno=1
						  then outrec_file(outrec_file.count+1).output_rec :=j.HeaderRecord;
                          end if;
						  j.HeaderRecord:=f$replace(j.HeaderRecord,rpad(vno_val,16,' '),'@@@@@@@@@@@@@@@@');
						  recno:=recno+1;
						  --
					      BudRec              :=f$GetBudRec(k.wol_bud_id);
						  ConRec              :=f$GetConRec(k.wol_id);
						  PayRec              :=f$GetPayRec(i.cp_payment_id,k.wol_id);
						  pi_net_amount       :=0;
						  pi_vat_amount       :=0;
						  pi_bud_cost_code    :='';
						  pi_gl_code          :='';
						  pi_internal_order_no:='';
						  pi_tax_code         :='';
						  pi_line_text        :='';
						--
					    pi_net_amount        :=PayRec.cp_payment_value;
						--
						if (substr(BudRec.bud_cost_code,1,1)='C' and substr(BudRec.bud_cost_code,1,2)<>'CP')
						then pi_bud_cost_code    :=f$spaces(10);
						     pi_gl_code          :=g_gl_code;
						     pi_internal_order_no:=BudRec.bud_cost_code;
						else pi_bud_cost_code    :=BudRec.bud_cost_code;
						     pi_gl_code          :=ConRec.con_external_ref;
						     pi_internal_order_no:=null;
						end if;
						--
						pi_vat_amount        :=f$GetVATValue(k.wol_id);
						-- To set the tax code we need to check to see if there are any BOQ items with the following codes:
						-- '100','VAT20','VAT15','VAT17.5','VAT5.0','CI' and the sum roral valyue of the BOQ's > 0
						-- VAT20 > 0 Tax Code = 'VR'
						-- VAT17 > 0 Tax Code = 'VS'
						-- VAT5  > 0 Tax Code = 'VL'
						-- ELSE      Tax Code = 'VO'
						pi_tax_code          :=f$GetVatTaxCode(k.wol_id);
                        pi_line_text         :=f$GetLineText(i.wol_works_order_no
						                                    ,k.wol_id
				                                            ,i.cp_payment_id
									                        ,'VNO');
					    RecLine:='C'
                           ||to_char(pi_net_amount,'S099999999999.99')
                           ||rpad(pi_bud_cost_code,10,' ')
                           ||lpad(pi_gl_code,10,0)
                           ||to_char(pi_vat_amount,'S099999999999.99')
                           ||lpad(pi_tax_code,2)
                           ||rpad(nvl(pi_internal_order_no,' '),12)
                           ||f$spaces(20)
                           ||rpad(pi_line_text,50)
                           ||f$spaces(21);
                           --
                           outrec_file(outrec_file.count+1).output_rec :=RecLine;
						   Brec_Net   :=Brec_Net   +pi_net_amount;
                           Brec_Vat   :=Brec_Vat   +pi_vat_amount;
                           --
						   if not f$IsTermContract(i.wol_works_order_no,'RWY')
						   then show('In here '||to_char(k.wol_id));
						        Brec_Labour:=f$GetLabourAct(k.wol_id);
								Show('Labour: '||to_char(Brec_Labour));
						        Brec_Net:=Brec_Net+Brec_Vat;
					            -- At this point the 'B' record needs to be added to the collection.
					            -- There will be 1 'B' record but potentially many work 'C' records ( Work Order Lines ).
					            RecLine:='BK'||to_char(Brec_Net   ,'S099999999999.99')
							    Brec_Net   :=0;
					                         ||to_char(Brec_Vat   ,'S099999999999.99')
								             ||to_char(Brec_Labour,'S099999999999.99');
								outrec_file(outrec_file.count+1).output_rec :=RecLine;
                                Brec_Vat   :=0;
                                Brec_Labour:=null;
						   end if;
					 end loop;
					 if f$IsTermContract(i.wol_works_order_no,'RWY')
					 then Brec_Net:=Brec_Net+Brec_Vat;
					      -- At this point the 'B' record needs to be added to the collection.
					      -- There will be 1 'B' record but potentially many work 'C' records ( Work Order Lines ).
					      RecLine:='BK'||to_char(Brec_Net   ,'S099999999999.99')
					                   ||to_char(Brec_Vat   ,'S099999999999.99')
						     	 	   ||to_char(Brec_Labour,'S099999999999.99');
					      outrec_file(outrec_file.count+1).output_rec :=RecLine;
						  Brec_Net   :=0;
                          Brec_Vat   :=0;
                          Brec_Labour:=null;
					 end if;
				  end loop;
        end loop;

        --
        -- So here we are going to run some data cursors that will be present in this procedure.
        -- The cursors will populate a pl.sql collection and then the contents of the collection
        -- will be output to a directory and file.
        --
        if not write_the_file(pi_file
                           ,  pi_dir)
        then hig_process_api.log_it(pi_message => 'Error{write_file(638)}: There was an issue outputting the specified datafile.');
        end if;
		--

   end if;
   --
END write_file;
--
-----------------------------------------------------------------------------
--
END x_cim_putfile;
/
