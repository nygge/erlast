%% @type ami_cmd() = absolute_timeout()| agent_callback_login()| agent_logoff() | agents() | change_monitor() | command() | db_get() | db_put() | events() | extension_state() | get_var() | hangup() | iax_netstats() | iax_peers() | list_commands() | login() | logoff() | mailboxcount() | mailbox_status() | monitor() | originate()  | parked_calls() | ping() | queue_add() | queue_pause() | queue_remove() | queues() | queue_status() | redirect() | set_cdr_user_field() | set_var() | sip_showpeer() | sip_peers() | status() | stop_monitor() | zap_dial_offhook() | zap_dnd_off() | zap_dnd_on() | zap_hangup() | zap_show_channels() | zap_transfer()

%% @type absolute_timeout() = #absolute_timeout{channel=string(),timeout=integer()}.
-record(absolute_timeout,{channel,timeout}).

%% @type agent_callback_login() = #agent_callback_login{agent=string(),exten=string(),context=string(),ackCall=string(),wrapUpTime=string()}.
-record(agent_callback_login,{agent,exten,context,ackCall,wrapUpTime}).

%% @type agent_logoff() = #agent_logoff{agent=string(),soft=boolean()}.
-record(agent_logoff,{agent,soft}).

%% @type agents() = #agents{}.
-record(agents,{}).

%% @type change_monitor() = #change_monitor{channel=string(),file=string()}.
-record(change_monitor,{channel,file}).

%% @type command() = #command{command=string()}.
-record(command,{command}).

%% @type db_get() = #db_get{family=string(),key=string()}.
-record(db_get,{family,key}).

%% @type db_put() = #db_put{family=string(),key=string(),value=string()}.
-record(db_put,{family,key,value}).

%% @type events() = #events{eventMask=string()}.
-record(events,{eventMask}).

%% @type extension_state() = #extension_state{extension=string(),context=string()}.
-record(extension_state,{extension,context}).

%% @type get_var() = #get_var{channel=string(),var=string()}.
-record(get_var,{channel,var}).

%% @type hangup() = #hangup{channel=string()}.
-record(hangup,{channel}).

%% @type iax_netstats() = #iax_netstats{}.
-record(iax_netstats,{}).

%% @type iax_peers() = #iax_peers{}.
-record(iax_peers,{}).

%% @type list_commands() = #list_commands{}.
-record(list_commands,{}).

%% @type login() = #login{user=string(),secret=string()}.
-record(login,{user,secret}).

%% @type logoff() = #logoff{}.
-record(logoff,{}).

%% @type mailboxcount() = #mailboxcount{mailbox=string()}.
-record(mailboxcount,{mailbox}).

%% @type mailbox_status() = #mailbox_status{mailbox=string()}.
-record(mailbox_status,{mailbox}).

%% @type monitor() = #monitor{channel=string(),file=string(),format=string(),mix=string()}.
-record(monitor,{channel,file,format,mix}).

%% @type originate() = #originate{channel=string()}.
-record(originate,{channel}).

%% @type parked_calls() = #parked_calls{}.
-record(parked_calls,{}).

%% @type ping() = #ping{}.
-record(ping,{}).

%% @type queue_add() = #queue_add{interface=string(),queue=string(),penalty=string(),pause=string()}.
-record(queue_add,{interface,queue,penalty,pause}).

%% @type queue_pause() = #queue_pause{interface=string(),queue=string(),penalty=string(),pause=string()}.
-record(queue_pause,{interface,queue,penalty,pause}).

%% @type queue_remove() = #queue_remove{interface=string(),queue=string()}.
-record(queue_remove,{interface,queue}).

%% @type queues() = #queues{}.
-record(queues,{}).

%% @type queue_status() = #queue_status{}.
-record(queue_status,{}).

%% @type redirect() = #redirect{channel=string(),extrachannel=string(),exten=string(),context=string(),priority=string()}.
-record(redirect,{channel,extrachannel,exten,context,priority}).

%% @type set_cdr_user_field() = #set_cdr_user_field{channel=string(),userfield=string(),append=string()}.
-record(set_cdr_user_field,{channel,userfield,append}).

%% @type set_var() = #set_var{channel=string(),variable=string(),value=string()}.
-record(set_var,{channel,variable,value}).

%% @type sip_showpeer() = #sip_showpeer{peer=string()}.
-record(sip_showpeer,{peer}).

%% @type sip_peers() = #sip_peers{}.
-record(sip_peers,{}).

%% @type status() = #status{channel=string()}.
-record(status,{channel}).

%% @type stop_monitor() = #stop_monitor{channel=string()}.
-record(stop_monitor,{channel}).

%% @type zap_dial_offhook() = #zap_dial_offhook{channel=string(),number=string()}.
-record(zap_dial_offhook,{channel,number}).

%% @type zap_dnd_off() = #zap_dnd_off{channel=string()}.
-record(zap_dnd_off,{channel}).

%% @type zap_dnd_on() = #zap_dnd_on{channel=string()}.
-record(zap_dnd_on,{channel}).

%% @type zap_hangup() = #zap_hangup{channel=string()}.
-record(zap_hangup,{channel}).

%% @type zap_show_channels() = #zap_show_channels{}.
-record(zap_show_channels,{}).

%% @type zap_transfer() = #zap_transfer{}.
-record(zap_transfer,{}).
