
Deploy repl (mmr) replication in a docker swarm

Steps:
1.  create a swarm.

    % docker swarm init

   then copy the join command that's printed and run that command
   on other machines which will be in the swarm

2. % make network

3. % make visualizer
    optional step.
    then you can go to   http://ipaddr:8080
    in a web browser to visualize the nodes in the swarm and
    what's running in them.  You'll see visualizer itself as
    the only running container

4. % make controlling
    this will start the controlling instance.
    you can go to   http://ipaddr:10035
    to log into this instance.
    Click on the repo 'myrepl' and  then
    click on  'Manage Replication Instances as controller'

    The visualizer will not show this container because it's not
    run as a service in the swarm.
    
5. % make copy
    This will create three agraphs each of which will connnect
    to the controller and replication the database myrepl
    If you refresh the http://ipaddr:10035 screen you'll
    be able to watch the state change as the three copies
    connect to the controlling instance


    
    


