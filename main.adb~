----------------------------csc410/prog5/as5.adb----------------------------
-- Author:	Matthew Bennett
-- Class:		CSC410 Burgess
-- Date:		11-01-04 							Modified: 11-15-04
-- Due:			11-16-04
-- Desc:		Assignment 5: LAMPORT'S ALGORITHM FOR VIRTUAL TOPOLOGY NETWORKS
--
--	a nonproduction implementation of
--	LAMPORT's "bakery" algorithm which utilizes clocks (a 'ticketing' system
--	like IN the bakery or at the dept of motor vehicles) to determine which
--	process may go into the critical section next.
--	
--	LAMPORT implemented as described IN
--  	"Algorithms FOR Mutual Exclusion", M. Raynal
--  	MIT PRESS Cambridge, 1986 ISBN: 0-262-18119-3
--		with additional revisions due to message passing across a virtual topology
----------------------------------------------------------------------------

----------------------------------------------------------------
-- dependencies

WITH ADA.TEXT_IO; 							USE ADA.TEXT_IO;
WITH ADA.INTEGER_TEXT_IO; 			USE ADA.INTEGER_TEXT_IO;
WITH ADA.NUMERICS.FLOAT_RANDOM;	USE ADA.NUMERICS.FLOAT_RANDOM;
WITH ADA.CALENDAR; 	-- (provides cast: natural -> time FOR input into delay)
WITH ADA.STRINGS; 							USE ADA.STRINGS;
WITH ADA.STRINGS.UNBOUNDED; 		USE ADA.STRINGS.UNBOUNDED;

PROCEDURE Main IS
	--GLOBALS VARS: randomPool, taskarray, go, stop

	randomPool : ADA.NUMERICS.FLOAT_RANDOM.GENERATOR;
	--random number pool FOR generating

	go, stop: Boolean 		:= FALSE; --allow FOR graceful execution and termination

	TYPE MESG IS (REQ, ACK, REL);
	PACKAGE MESG_IO IS NEW Enumeration_IO(MESG); USE MESG_IO;
	--so that we can natively output enumerated type

	MAX_NEIGHBORS : CONSTANT := 30;
	--maximum number of neighbors, FOR efficiency IN array passing
	-- 1/2 of this num is the same as euler # FOR a graph

	MAX_TASKS 		: CONSTANT := 20;
	--maximum number of neighbors, FOR efficiency IN array passing
	
	TYPE type_message IS (REQ, ACK, REL); --request, acknowledge, release
	PACKAGE ENUM_IO IS NEW Enumeration_IO(type_message); Use ENUM_IO;
	--allow input and output of our enumerated names

	TYPE RX_TASK;
	TYPE RX_Ptr IS ACCESS RX_TASK;
	--forward declaration needed FOR access type to RX_TASK FOR array of access es

	TYPE passableArray IS ARRAY (0..MAX_NEIGHBORS) OF Integer;
	--this is needed bc anonymous types are not allowed IN declarations
	--specifically, so that we can pass arrays around between entries as arguments

	taskArray : ARRAY (0..MAX_NEIGHBORS) OF RX_Ptr;
		--keep up with tasks thrown off

-- Receive/listener task
TASK TYPE RX_TASK IS
	ENTRY Start( id_IN : IN Integer; Neighbors_IN : IN passableArray); 
	--initialize variables

	ENTRY FWD(dest: Integer; msg: type_message; k: Integer; j: Integer);
	--method used to propogate messages through the network until dest = self

  ENTRY kill;
	--kill off the task, politely ask it to die

	ENTRY REQUEST;
	ENTRY ACKNOWLEDGE;
	ENTRY RELEASE;

END RX_TASK;

-- BEGIN Receive TASK Definition -- 
TASK BODY RX_TASK IS

  TYPE message IS
	RECORD
		mestype: type_message := rel;
		clock: Integer := 0;
		id: Integer;
	END RECORD;

	queue: ARRAY (0 .. MAX_TASKS) OF message;
	--the distributed queue of messages

	Neighbors : passableArray;
	--keeps track of who task can send to, receive from

	id				: Integer;
	--self identification

  outp				: Unbounded_String := Null_Unbounded_String;
	--temporary string variable FOR uninterrupted output

	friends			: ARRAY(0..MAX_NEIGHBORS) OF RX_PTR;
	--used FOR calling on receiver tasks

	osn, tempOSN : Integer := 0;
	--"own sequence number", Lamport clock FOR task

   temp1, temp2: message;
   FOR_all: boolean := TRUE;
   dead: boolean := FALSE;
	--temporary variables

TASK TYPE TX_TASK (
--seperate task, so it can be spawned any time!
	dest: Integer; --destination IN network
	mess: MESG;	--the message itself
	clock: Integer; --the sequence number of the sending process
	i: Integer --sending process id #
	)
IS --seperate task, so we can spawn a send -whenever- asynchronously
--ge that was short
END TX_TASK;

	TYPE TX_PTR IS ACCESS TX_TASK;
	myTX : TX_PTR; --so we can launch transmit task anytime

TASK BODY TX_TASK IS
BEGIN

null;

END TX_TASK; --definition

TASK TYPE AL_TASK
--gee, that was short
IS END AL_TASK;

--internal task, FOR lamport's algorithm
TASK BODY AL_TASK IS
BEGIN

LOOP
	FOR index IN 0 .. MAX_NEIGHBORS LOOP
		queue(index).id := index;
	END LOOP;

	EXIT WHEN (go);
END LOOP;
--intitialize the queue so that each element stores the proper process id

LOOP
 --broadcast
	FOR I IN 0..n
	LOOP
		IF(I /= id)THEN tsk_TX := new transmit(I, req, local_clock, id); END IF;
	END LOOP;

	q(id) := (req, local_clock, id);

	osn := osn + 1;
	tempOSN := osn;

	wait:
	LOOP
		FOR j IN 0..n
		LOOP
			IF j /= id THEN
				IF((q(id).clock < q(j).clock)
					OR ((q(id).clock = q(j).clock) AND (q(id).id < q(j).id)))
				THEN null;
				ELSE FOR_all := FALSE;
				END IF;
			END IF;
		END LOOP;

	EXIT wait WHEN (FOR_all = TRUE);

	FOR_all := TRUE;

	END LOOP wait;

	EXIT WHEN dead = TRUE;

	outp := (((80/6)*id) * " ") & Integer'Image(id) & " IN CS.";
	Put(To_String(outp)); New_line;

	delay Duration(float(random(G)) + float(10));
	st := (((80/(n+1))*id) * " ") & Integer'Image(id) & " out CS.";
	Put_line(To_String(st));

	-- broadcast
	local_clock := osn;
	FOR I IN 0..n
	LOOP
		IF(I /= id)THEN
			tsk_TX := new transmit(I, rel, local_clock, id);
		END IF;
	END LOOP;

	q(id) := (rel, local_clock, id);
	osn := osn + 1;
	local_clock := osn;

	EXIT WHEN dead = TRUE;

	END LOOP;
END AL_TASK;

TYPE AL_Ptr IS ACCESS AL_TASK;
aPtr : AL_Ptr;
--END of internal task, FOR lamport's algorithm

-- beginnig of RX_TASK definition	
BEGIN 
	-- ACCEPT creation messages -- 

ACCEPT Start (
	id_IN 				: IN Integer;
	Neighbors_IN 	: IN passableArray
)
DO

	--initialize neighbors array
	FOR I IN Neighbors'First .. Neighbors'Last
	LOOP
		Neighbors(I) := Neighbors_IN(I);
	END LOOP; 
	
	id := id_IN;	

END Start;

ACCEPT KILL
DO
null;
END KILL;

ACCEPT FWD(dest: Integer; msg: type_message; k: Integer; j: Integer)
DO
null;
END FWD;

SELECT

ACCEPT REQUEST
DO
	null;
END REQUEST;

ACCEPT ACKNOWLEDGE --acknkowledge
DO
	null;
END ACKNOWLEDGE;

ACCEPT RELEASE
DO
	null;
END RELEASE;

END SELECT;

-- RX_TASK definition
	aPtr := new AL_TASK; -- spin off lamport task

	-- Start Message Receiving LOOP --
--LOOP
null;	
--END LOOP; -- RX LOOP

END RX_TASK;

PROCEDURE Driver IS

	seedUser		: Integer;
	--user input random seed FOR random number generator

	infile 			: FILE_TYPE;
	--ada.standard.textIO type FOR reading ascii and iso-8XXX

	filename		: string(1..5);
	--what file should we read from?

--Following are variables FOR building logical network topologies	

	taskId 			: Integer;
	--temporary FOR keeping track of which task we are reading IN

	neighbors		: passableArray;
	--array of neighbors to be passed into a node upon initialization	

	neighborCount : Integer;
	--temporary to keep up with number of neighbors FOR a certain task/node

--killing time variables
	toKill			: Integer;
	--assigned a random ID to determine which process to kill next
	
	dead				: ARRAY (0..MAX_TASKS) of Boolean := (Others => FALSE);
	--keeps track of which processes have been slain, so we dont try to kill a 
		--process twice , which would raise an exception

BEGIN
	put_line("Lamport's Algorithm");

	put("# random seed:       ");
	get(seedUser); --to  ensure a significantly random series, a seed is needed
									-- to generate pseudo-random numbers
 	Ada.Numerics.Float_Random.Reset(randomPool,seedUser);
		--seed the random number pool

	put("Filename:            ");
	get(filename);
	--first lets read IN the 
	Open (File=> inFile, Mode => IN_FILE, Name => filename);
	--open as read only ascii and use reference infile
	--file format is:	nodeID neighbor neighbor neighbor...neighbor[MAX_NEIGHBORS]

	WHILE NOT END_OF_FILE(infile) 
	LOOP
		neighborCount := 0;

	--receive file input
		Get(infile, TASKId);

		WHILE NOT END_OF_LINE(infile) --there is routing information on the line
		LOOP
			Get(infile, neighbors(neighborCount) );
			neighborCount := neighborCount + 1;
		END LOOP;

		--we can have one tight LOOP now since this time
		-- all neighbors are known ahead of time
		--create and initialize part FOR nodes/tasks
		taskArray(TASKId) := new RX_TASK;
		taskArray(TASKId).start(taskId, neighbors);

	END LOOP; --END of file reading LOOP

	go := TRUE;

--	DELAY Duration(60.0); --allow things to run 1 minute before doom

	Put("Number of tasks: "); Put (taskID); new_line;

  --kill off random processes
	FOR kill_index IN 0 .. (TaskID) --FOR as many as there are tasks
	LOOP
    delay (1.0);
		Put ("Going to kill random process ... ");
		toKill := (Integer(random(randomPool)) MOD TaskID);

		WHILE (dead(toKill))
		LOOP 			--iterate until process toKill isn't one that is already dead!
			toKill := toKill + 1 MOD TaskID; --random didnt cut it, try the next one
		END LOOP;

		Put (toKill); new_line;

		taskArray(toKill).kill; --kill off our random process
		dead(toKill) := TRUE;

  END LOOP; --END LOOP to kill out all processes

	stop := TRUE;

  Close(infile);

	EXCEPTION
   WHEN Name_Error =>
      Put(Item => "File not found.");
   WHEN Status_Error =>
      Put(Item => "File already open.");
   WHEN Use_Error =>
      Put(Item => "You lack permission to open file");
--   WHEN constraint_Error =>
--      Put(Item => "problem IN code! constraint error thrown");

END Driver;

BEGIN Driver; END Main; --seperation of global vars
