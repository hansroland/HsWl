First Buffers to send:

In Binary:

In Binary:
[0.001791593s] Request sender=1  op=0  size=12  03000000
[0.001791593s] Request sender=1  op=1  size=12  02000000
[0.001961068s] Event   sender=1  op=1  size=12  03000000
[0.001961068s] Event   sender=3  op=0  size=12  320a0000
[0.001961068s] Event   sender=2  op=0  size=32  24000000 0a000000 776c5f6f 75747075 74000000 03000000
[0.001961068s] Event   sender=2  op=0  size=44  23000000 18000000 7a77705f 706f696e 7465725f 67657374 75726573 5f763100 01000000
[0.001961068s] Event   sender=2  op=0  size=28  22000000 08000000 776c5f73 65617400 07000000
[0.001961068s] Event   sender=2  op=0  size=64  21000000 2a000000 7a77705f 6b657962 6f617264 5f73686f 72746375 74735f69 6e686962 69745f6d 616e6167 65725f76 31000000 01000000


In simple:
[0.069512057s]  -> wl_display@1.sync(new id wl_callback@3)
[0.069512057s]  -> wl_display@1.get_registry(new id wl_registry@2)
[0.085502778s] <-  wl_display@1.delete_id(3)
[0.085502778s] <-  wl_callback@3.done(1262)
[0.085502778s] <-  wl_registry@2.global(36, "wl_output", 3)
[0.085502778s] <-  wl_registry@2.global(35, "zwp_pointer_gestures_v1", 1)

In Json:
{"message":{"arguments":[{"name":"callback","value":{"type":"NewId","value":3}}],"interface":"wl_display","name":"sync","object":1,"type":"Request"},"timestamp":"0.065552318s"}
{"message":{"arguments":[{"name":"registry","value":{"type":"NewId","value":2}}],"interface":"wl_display","name":"get_registry","object":1,"type":"Request"},"timestamp":"0.065552318s"}
{"message":{"arguments":[{"name":"id","value":{"type":"UInt","value":3}}],"interface":"wl_display","name":"delete_id","object":1,"type":"Event"},"timestamp":"0.065722911s"}
{"message":{"arguments":[{"name":"callback_data","value":{"type":"UInt","value":1300}}],"interface":"wl_callback","name":"done","object":3,"type":"Event"},"timestamp":"0.065722911s"}
{"message":{"arguments":[{"name":"name","value":{"type":"UInt","value":36}},{"name":"interface","value":{"type":"String","value":"wl_output"}},{"name":"version","value":{"type":"UInt","value":3}}],"interface":"wl_registry","name":"global","object":2,"type":"Event"},"timestamp":"0.065722911s"}

