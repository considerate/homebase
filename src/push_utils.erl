-module(push_utils).
-export([connect_to_apple_push_notification_service/0])

connect_to_apple_push_notification_service()->
    Address = "gateway.sandbox.push.apple.com",
    Port = 2195,
    Timeout = 1000,
    Options = [{certfile, "cert.pem"},{keyfile,"key-noenc.pem"},{mode, binary}],
    {ok,Socket} = ssl:connect(Address,Port,Options,Timeout),
    Socket.
