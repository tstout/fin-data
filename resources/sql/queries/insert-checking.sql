MERGE INTO FINKRATZEN.CHECKING 
(ID, POSTING_DATE, AMOUNT, MERCHANT)
KEY(ID) 
VALUES (?, ?, ?, ?);
