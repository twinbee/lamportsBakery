----------------------------csc410/prog5/as5.adb----------------------------
-- Author:	Matthew Bennett
-- Class:		CSC410 Burgess
-- Date:		11-01-04 							Modified: 11-01-04
-- Due:			11-7-04
-- Desc:		Assignment 5: LAMPORT'S ALGORITHM FOR VIRTUAL TOPOLOGY NETWORKS
--
--	a nonproduction implementation of
--	LAMPORT's "bakery" algorithm which utilizes clocks (a 'ticketing' system
--	like in the bakery or at the dept of motor vehicles) to determine which
--	process may go into the critical section next.
--	
--	LAMPORT implemented as described in
--  	"Algorithms FOR Mutual Exclusion", M. Raynal
--  	MIT PRESS Cambridge, 1986 ISBN: 0-262-18119-3
--		with additional revisions due to message passing across a virtual topology
----------------------------------------------------------------------------

-- Refactorings  10-05-04: (deNOTed @FIX@)
--		(4) linked list of processes instread of array

----------------------------------------------------------------
-- dependencies

WITH ADA.TEXT_IO; 							USE ADA.TEXT_IO;
WITH ADA.INTEGER_TEXT_IO; 			USE ADA.INTEGER_TEXT_IO;
WITH ADA.NUMERICS.FLOAT_RANDOM;	USE ADA.NUMERICS.FLOAT_RANDOM;
WITH ADA.CALENDAR; 	-- (provides cast: natural -> time FOR input into delay)
WITH ADA.STRINGS; 						USE ADA.STRINGS;

PROCEDURE Main IS
	G : Generator;
	MAX_NEIGHBORS : Constant := 25;

	TYPE RX_TASK;
	TYPE RX_Ptr IS ACCESS RX_TASK;

	TYPE Myarray IS ARRAY (0..MAX_NEIGHBORS) of Integer;
		--this is needed bc anonymous types are not allowed in declarations

	TYPE Node;
	TYPE Node_Ptr IS ACCESS Node;
	TYPE Node IS RECORD
		m_Value : Integer;
		Next : Node_Ptr := NULL;
		Prev : Node_Ptr := NULL;
	END RECORD;

	TASKarray : ARRAY (0..MAX_NEIGHBORS) of RX_Ptr;
		--keep up with tasks thrown off

PROCEDURE Enqueue (Head : IN OUT Node_Ptr;
									Value : IN Integer) IS

Item : Node_Ptr;

BEGIN
	Item := new	Node;
	Item.m_Value := Value;

	IF (Head = NULL) THEN	Head := Item;
	ELSE -- Insert at the beginning
		Item.Next := Head;
		Head.Prev := Item;
		Head := Item;	
	END IF;
END Enqueue;

PROCEDURE Dequeue(Head : IN out Node_Ptr; Value : out Integer) IS	Curr : Node_Ptr;
BEGIN
	Curr := Head;

	IF (Head = NULL) THEN -- We have an empty queue
	put ("Error : Empty Queue Encountered");
	ELSE

		WHILE (Curr.Next /= NULL) --iterate to end of list
		LOOP
			Curr := Curr.Next;
		END LOOP;

		IF (Curr.Prev = NULL) THEN Head := NULL;
		ELSE Curr.Prev.Next := NULL;	END IF;

		Value := Curr.m_Value;
	END IF;
END Dequeue;

FUNCTION IsEmpty (Head : Node_Ptr) RETURN Boolean IS
BEGIN
	IF (Head = NULL) THEN RETURN TRUE;
	ELSE RETURN FALSE; END IF;
END IsEmpty;

-- BEGIN Receive TASK Declaration --
TASK TYPE RX_TASK IS
	ENTRY Start( myid : Integer; hold : Integer; Neighbor : Myarray); 
	ENTRY Send (mesgTYPE : Character; FromId : Integer; ToId : Integer);
END RX_TASK;

-- BEGIN Receive TASK Definition -- 
TASK BODY RX_TASK IS
	Neighborarray : ARRAY (0..MAX_NEIGHBORS) of Integer;
	HOLDER : Integer;
	USING : Boolean := FALSE;
	REQUEST_Q : Node_Ptr := NULL;
	ASKED : Boolean := FALSE;
	SELF : Integer;
	TYPE Message;
	TYPE Message_Ptr IS ACCESS Message;
	TYPE Message IS RECORD
		m_mesgTYPE : Character;
		m_fromId : Integer;
		m_toId : Integer;
		Next,Prev : Message_Ptr := NULL;
	END RECORD;
	MESG_Q : Message_Ptr := NULL;
	
	-- FUNCTIONs FOR Enqueuing AND Dequeuing a Message Queue --------------
PROCEDURE Message_Enqueue(
	Head : IN OUT Message_Ptr;
	mesgTYPE : Character;
	fromId : Integer;
	toId : Integer) IS

Item : Message_Ptr;

BEGIN
	Item := new Message;
	Item.m_mesgTYPE := mesgTYPE;
	Item.m_fromId := fromId;
	Item.m_toId := toId;
  
	IF (Head = NULL) THEN -- We have an empty queue
		Head := Item;
	ELSE -- Insert at the beginning
		Item.Next := Head;
		Head.Prev := Item;
		Head := Item;
	END IF;
END Message_Enqueue;


PROCEDURE Message_Dequeue(
	Head : IN out Message_Ptr;
	tempMesg : out Message)
IS Curr : Message_Ptr;

BEGIN
	Curr := Head;
	IF (Head = NULL) THEN -- We have an empty queue
		put ("Error : Empty Message Queue Encountered");
	ELSE
		WHILE (Curr.Next /= NULL) LOOP Curr := Curr.Next; END LOOP;
    -- Curr should now point to the last element IN the list --
    IF (Curr.Prev = NULL)
			THEN Head := NULL;
    ELSE Curr.Prev.Next := NULL;
		END IF;

		tempMesg.m_mesgTYPE := Curr.m_mesgTYPE;
		tempMesg.m_fromId := Curr.m_fromId;
		tempMesg.m_toId := Curr.m_toId;
	END IF;
END Message_Dequeue;

FUNCTION Message_IsEmpty (Head : Message_Ptr) RETURN Boolean IS
BEGIN
IF (Head = NULL) THEN RETURN TRUE;
	ELSE RETURN FALSE;
END IF;
END Message_IsEmpty;

	-- FUNCTIONs FOR ASSIGN_PRIVILEGE AND MAKE REQUEST ---------
PROCEDURE ASSIGN_PRIVILEGE IS
BEGIN

IF (HOLDER = SELF) AND (NOT USING) AND (NOT IsEmpty(REQUEST_Q)) THEN

	Dequeue(REQUEST_Q, HOLDER);
	ASKED := FALSE;
	IF HOLDER = SELF THEN
		USING := TRUE;
		-- Critical Section
			Put (SELF, (1+(4*SELF))); Put (" IN CS"); New_Line;
			delay (Standard.Duration(Random(G) * 4.0));
			Put (SELF, (1+(4*SELF))); Put (" out CS"); New_Line;
	ELSE
		TASKarray(HOLDER).Send('P', SELF, HOLDER); 
		
	END IF;
END IF;

END ASSIGN_PRIVILEGE;



PROCEDURE MAKE_REQUEST IS
BEGIN
IF (HOLDER /= SELF) AND (NOT IsEmpty(REQUEST_Q)) AND (NOT ASKED) THEN
	TASKarray(HOLDER).Send('R', SELF, HOLDER);
	ASKED := TRUE;
END IF;
END MAKE_REQUEST;


TASK TYPE AL_TASK IS END AL_TASK;
TASK BODY AL_TASK IS

currMessage : Message;

BEGIN
LOOP
	-- Only attempt to enter the critical section 35% of time
	IF ((Random(g) * 10.0) > 7.0) THEN
		Delay (Standard.Duration(Random(G) * 3.0));
  	-- Node wishes to enter the critical Section --
		Enqueue(REQUEST_Q, SELF);
		ASSIGN_PRIVILEGE;
		MAKE_REQUEST;
		--	Node exits the critical section --
  	USING := FALSE;
		ASSIGN_PRIVILEGE;
		MAKE_REQUEST;
	END IF;
  
	-- Process any messages IN the message queue --
	IF (NOT Message_IsEmpty(MESG_Q)) THEN
		Message_Dequeue(MESG_Q, currMessage);
		case currMessage.m_mesgTYPE IS
			WHEN 'P' =>
			BEGIN
				HOLDER := SELF;
				ASSIGN_PRIVILEGE;
				MAKE_REQUEST;
			END;
			
			WHEN 'R' =>
			BEGIN
				Enqueue(REQUEST_Q, currMessage.m_FromId);
				ASSIGN_PRIVILEGE;
				MAKE_REQUEST;
			END;
			
			WHEN Others => NULL; --make ada happy

		END Case;
	  
	ELSE -- We delay a bit longer, as to let others through
		Delay (Standard.Duration(Random(G) * 6.0)); 	
  END IF;
		
	END LOOP;

	END AL_TASK;

	TYPE AL_Ptr IS ACCESS AL_TASK;
	aPtr : AL_Ptr;
	--------------------------------------------------------------
	
-- BEGIN THE RECEIEVE TASK DEFINITION --
BEGIN 
	-- ACCEPT creation messages -- 

ACCEPT Start(
		myid : Integer;
		hold : Integer;
		Neighbor : Myarray)
DO

	FOR I IN Neighbor'First..Neighbor'Last
	LOOP
		Neighborarray(I) := Neighbor(I);
	END LOOP; 
	
	HOLDER := hold;
	SELF := myid;	

END Start;

	aPtr := new AL_TASK; -- Start Algorithm LOOP

	-- Start Message Receiving LOOP --
LOOP

ACCEPT Send (mesgTYPE : Character; FromId : Integer; ToId : Integer)
DO 
	Message_Enqueue (MESG_Q,mesgTYPE, fromId, ToId);
	put (SELF, (1+(4*SELF))); 
	
	IF (MesgTYPE = 'P') THEN put (" Priv ");
	ELSE put (" Request ");	END IF;

	Put (FromId, 0);
	Put (","); Put (ToId,0); New_Line;

END Send;

END LOOP; -- RX LOOP

END RX_TASK;

PROCEDURE Driver IS

	infile 			: FILE_TYPE; --ada.standard.textIO type for reading ascii and iso-8XXX
	taskId 			: Integer;
	holder 			: Integer;
	neighArray	: myArray;
	neighCount : Integer := 0;

BEGIN
	Open (inFile, IN_FILE, "input.txt");
	--open as read only ascii and use reference infile
	--file format is:	nodeID neighbor neighbor neighbor ...neighbor[MAX_NEIGHBORS]

	WHILE NOT END_OF_FILE(infile) 
	LOOP
		Get(infile, TASKId);
		Get(infile, Holder);
		WHILE NOT END_OF_LINE(infile) --there are neighbors coming on the line
		LOOP
			Get( infile, neighArray(neighCount) );
			neighCount := neighCount + 1;
		END LOOP;
		TASKarray(TASKId) := new RX_TASK;
		TASKarray(TASKId).Start(taskId, holder, neighArray);
	END LOOP;

END Driver;

BEGIN Driver; END Main;
