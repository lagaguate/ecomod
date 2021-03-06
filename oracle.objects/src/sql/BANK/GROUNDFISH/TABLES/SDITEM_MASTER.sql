--------------------------------------------------------
--  DDL for Table SDITEM_MASTER
--------------------------------------------------------

  CREATE TABLE "GROUNDFISH"."SDITEM_MASTER" 
   (	"PREYITEMCD" NUMBER(4,0), 
	"PREYITEM" VARCHAR2(20 BYTE), 
	"PREYSPECIES" VARCHAR2(50 BYTE), 
	"PREYSPECCD" NUMBER(4,0)
   ) PCTFREE 10 PCTUSED 40 INITRANS 1 MAXTRANS 255 NOCOMPRESS LOGGING
  STORAGE(INITIAL 32768 NEXT 65536 MINEXTENTS 1 MAXEXTENTS 2147483645
  PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1 BUFFER_POOL DEFAULT)
  TABLESPACE "MFD_GROUNDFISH" ;
 

   COMMENT ON COLUMN "GROUNDFISH"."SDITEM_MASTER"."PREYITEMCD" IS 'Food Item Code';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDITEM_MASTER"."PREYITEM" IS 'Food Item Group';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDITEM_MASTER"."PREYSPECIES" IS 'Food Item detail/species';
 
   COMMENT ON COLUMN "GROUNDFISH"."SDITEM_MASTER"."PREYSPECCD" IS 'Food Item detail/species code';
 
   COMMENT ON TABLE "GROUNDFISH"."SDITEM_MASTER"  IS 'Prey Item master conversion and description - includes archival codes';
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "MFLIB" WITH GRANT OPTION;
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "BMACE";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "HARRISLE";
 
  GRANT UPDATE ON "GROUNDFISH"."SDITEM_MASTER" TO "HARRISLE";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "VDC";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "VDC_DEV";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "MFD_STOMACH";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "RICARDD";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "HUBLEYB";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "MACDONALDD";
 
  GRANT SELECT ON "GROUNDFISH"."SDITEM_MASTER" TO "GREYSONP";
