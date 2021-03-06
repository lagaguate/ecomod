--------------------------------------------------------
--  DDL for Table SDINF_EDIT
--------------------------------------------------------

  CREATE TABLE "GROUNDFISH"."SDINF_EDIT" 
   (	"DATASOURCE" VARCHAR2(3 BYTE), 
	"MISSION" VARCHAR2(15 BYTE), 
	"SETNO" NUMBER(3,0), 
	"SDATE" DATE, 
	"STIME" NUMBER(4,0), 
	"SLAT" NUMBER, 
	"SLONG" NUMBER, 
	"STRAT" VARCHAR2(3 BYTE), 
	"NAFO" VARCHAR2(10 BYTE), 
	"BOTTOM_TEMPERATURE" NUMBER(5,2), 
	"DEPTH" NUMBER(4,0), 
	"STATUS_FLAG" NUMBER
   ) PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  STORAGE(INITIAL 81920 NEXT 65536 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1 BUFFER_POOL DEFAULT)
  TABLESPACE "MFD_GROUNDFISH" ;
 

   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."DATASOURCE" IS 'Trip type code';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."MISSION" IS 'Trip Id';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."SETNO" IS 'Set Number';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."SDATE" IS 'Set date';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."STIME" IS 'Set time (24hr)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."SLAT" IS 'Set latitude (DDMM.MM)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."SLONG" IS 'Set longitude (DDMM.MM)';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."STRAT" IS 'Stratum';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."NAFO" IS 'NAFO division';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."BOTTOM_TEMPERATURE" IS 'Water temperature in degrees Celsius';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."DEPTH" IS 'bottom depth';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDINF_EDIT"."STATUS_FLAG" IS 'row status';
 
   COMMENT ON TABLE "GROUNDFISH"."SDINF_EDIT"  IS 'Stomach database trip information ';
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "MFLIB" WITH GRANT OPTION;
 
  GRANT DELETE ON "GROUNDFISH"."SDINF_EDIT" TO "HARRISLE";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "HARRISLE";
 
  GRANT UPDATE ON "GROUNDFISH"."SDINF_EDIT" TO "HARRISLE";
 
  GRANT DELETE ON "GROUNDFISH"."SDINF_EDIT" TO "SCOTTS";
 
  GRANT INSERT ON "GROUNDFISH"."SDINF_EDIT" TO "SCOTTS";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "SCOTTS";
 
  GRANT UPDATE ON "GROUNDFISH"."SDINF_EDIT" TO "SCOTTS";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "VDC";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "VDC_DEV";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "MFD_STOMACH";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "RICARDD";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "HUBLEYB";
 
  GRANT SELECT ON "GROUNDFISH"."SDINF_EDIT" TO "GREYSONP";
