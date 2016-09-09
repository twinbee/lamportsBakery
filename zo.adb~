-- With in my Text io package for put/get
With Text_IO; Use Text_IO;

-- With in Random float package for my random number generator
With Ada.Numerics.Float_Random; Use Ada.Numerics.Float_Random;

-- With in Unbounded strings for my string monipulation
With Ada.Strings.Unbounded; Use Ada.Strings.Unbounded;

-- Begining of lamport_algorithm code
Procedure zo is

type type_message is (req, ack, rel);

-- Instantiating integer IO.
Package INT_IO is new Integer_IO(Integer); Use INT_IO;

Package MSG_IO is new Enumeration_IO(type_message); Use MSG_IO;
-- This is my random variable.
G: Generator;

-- random seed
seed: Integer;

type receiver;

-- This is the type for my pointer variable for my receiver process.
type rec is Access receiver;

type rec_array_type is array (0..20) of rec;

all_systems_go, all_dead: boolean := false;

-- This is the declaration for my receiver type task.
task type receiver(id: Integer; n: Integer) is
   Entry start(friend_array: rec_array_type);
   Entry RX(dest: Integer; msg: type_message; k: Integer; j: Integer);
   Entry kill;
end receiver;

-- This is the intialization or body of the receiver type and
-- this is the code that the processeses will be excuting.
task Body receiver is

   type message;
   type message is
   record
       mestype: type_message := rel;
       clock: Integer := 0;
       id: Integer;
   end record;

   task type transmit(dest: Integer; mess: type_message; clock:
Integer; i: Integer) is
   end transmit;

   friends: rec_array_type;

   st: Unbounded_String := Null_Unbounded_String;

   --mestype: type_message;

   type TX is access transmit;
   tsk_TX: TX;

   osn, local_clock: Integer := 0;
   q: array (0 .. n) of message;

   temp1, temp2: message;

   for_all: boolean := true;
   dead: boolean := false;

   task algorithm;
   task body algorithm is

       begin
       loop
           for index in 0 .. n loop
               q(index).id := index;
           end loop;
           exit when (all_systems_go = true);
       end loop;

       loop
           --broadcast
           for I in 0..n loop
               if(I /= id)then
                   tsk_TX := new transmit(I, req, local_clock, id);
               end if;
           end loop;

           q(id) := (req, local_clock, id);

           osn := osn + 1;
           local_clock := osn;

           wait:
           loop
               for j in 0..n loop
                   if j /= id then
                       if((q(id).clock < q(j).clock)or((q(id).clock =
q(j).clock)and(q(id).id < q(j).id)))then
                           null;
                       else
                           for_all := false;
                       end if;
                   end if;
               end loop;
               exit wait when (for_all = true);
               for_all := true;
           end loop wait;

           exit when dead = true;

           -- This is my CS.  Which ever process made it to this section will
           -- get to do all of this wonderful stuff.  I am cating my string for
           -- printing.  After that small section I basicly do the samething
           -- again but this time i print process # " out CS".  I also have a
           -- delay in there just to add more randomness.

           st := (((80/(n+1))*id) * " ") & Integer'Image(id) & " in CS.";

           Put(To_String(st)); New_line;

           delay Duration(float(random(G)) + float(10));

           st := (((80/(n+1))*id) * " ") & Integer'Image(id) & " out CS.";

           Put_line(To_String(st));

           -- broadcast
           local_clock := osn;
           for I in 0..n loop
               if(I /= id)then
                   tsk_TX := new transmit(I, rel, local_clock, id);
               end if;
           end loop;

           q(id) := (rel, local_clock, id);
           osn := osn + 1;
           local_clock := osn;

           exit when dead = true;

           end loop;
   end algorithm;

   task body transmit is
       Begin
           friends(dest).RX(dest, mess, clock, i);
           st := ((((80/(n+1))*id)* " ") & Integer'Image(i) & "TX  " &
type_message'image(mess) & integer'image(dest));
           Put_line(to_string(st));
   end transmit;

   Begin

   loop

       select

       accept start(friend_array: rec_array_type)do

           friends := friend_array;

       end start;

      or

      accept RX (dest: Integer; msg: type_message; k: Integer; j:Integer) do
           if(dest /= id)then
               st := ((((80/(n+1))*id)*" ")&integer'image(id)&"FWD "
&type_message'image(msg) & integer'image(dest)&" FROM "&integer'image(j));
               put_line(to_string(st));
               tsk_TX := new transmit(dest, msg, k, j);
           else

               if(osn < k)then
                   osn := k;
               end if;

               osn := osn + 1;

               if(msg = req)then
                   q(j) := (req, k, j);
                   tsk_TX := new transmit(j, ack, osn, id);
               elsif (msg = rel)then
                   q(j) := (rel, k, j);
               elsif(msg = ack)then
                   if (q(j).mestype /= req) then
                       q(j) := (ack, k, j);
                   end if;
               end if;
               st := ((((80/(n+1))*id)*" ")&integer'image(id) & "RX  " &
type_message'image(msg) & integer'image(j));
               put_line(to_string(st));
           end if;
       end RX;
       or
       accept kill do
           st := ((((80/(n+1))*id)*" ")&integer'image(id)&" DIE");
       put_line(to_string(st));

           dead := true;

           -- broadcast
           local_clock := osn;
           for I in 0..n loop
               if(I /= id)then
                   tsk_TX := new transmit(I, rel, local_clock, id);
               end if;
           end loop;

           st := (((80/(n+1))*id*" ")&integer'image(id)&" DED");

           put_line(to_string(st));

       end kill;

       end select;

       exit when all_dead = true;
   end loop;
end  receiver;
-------------------------------------------------------------------------------
type FILE_MODE is (IN_FILE, OUT_FILE);

-- This variable is used to send the number of processes.
p_count: Integer;

pro_array: array(0..20) of rec;

friend_array: rec_array_type;

friend_file: FILE_TYPE;

-- number to keep the for loops correct
n: Integer;

index, victim, buddy: Integer;

clock: Integer := 0;

	toKill			: Integer;
	--assigned a random ID to determine which process to kill next
	
	dead				: ARRAY (0..20) of Boolean := (Others => FALSE);
	--keeps track of which processes have been slain, so we dont try to kill a 
		--process twice , which would raise an exception

-- to hold number of victim
--victim: Integer;

-- This is the start of my main section of code that does the prompt for how
-- many loops, what process to turn is set to and how many processes.  This
-- is also the section for the process calls.
Begin   -- lamport_algorithm

   open(friend_file, IN_FILE, "star1");

   seed := 997;

   get(friend_file, p_count);

   skip_line(friend_file);

   reset(G, seed);

   n := p_count - 1;

   -- Loop to create the total number of processes that were put in.
   for pid in 0 .. n loop
       pro_array(pid) := new receiver(pid, n);
   end loop;

   for I in 0 .. n loop
       while (not(END_OF_LINE(friend_file))) loop
               get(friend_file, index);
           get(friend_file, buddy);
               friend_array(index) := pro_array(buddy);
       end loop;

       skip_line(friend_file);

       pro_array(I).start(friend_array);
   end loop;

   all_systems_go := true;

	FOR kill_index IN 1 .. (p_count) --for as many as there are tasks
	LOOP
    delay (60.0);
		Put ("Going to kill random process ... ");
		toKill := (Integer(random(g)) MOD p_count);

		WHILE (dead(toKill))
		LOOP 			--iterate until process toKill isn't one that is already dead!
			toKill := (toKill + 1) MOD p_count; --random didnt cut it, try the next one
		END LOOP;

		Put (toKill); new_line;

		pro_Array(toKill).kill; --kill off our random process
		dead(toKill) := TRUE;

  END LOOP; --end loop to kill out all processes

   all_dead := true;

   close(friend_file);

end zo;    -- lamport_algorithm
