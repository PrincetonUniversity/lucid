## threads.dpt

Lucid has a serial processing model: the entire body of the current event's handler executes before processing for the next event begins. In other words, handlers execute atomically. The compiler pipelines a Lucid program to exploit parallelism while maintaining these convenient semantics.

This is a natural model for a packet processor, which we often think of as a single thread of operation that processes one packet at a time to completion, without interruption from other threads. But programs also need perform other "background" tasks. For example, a switch might need to: periodically scan through a counter table and delete old entries; figure out where to insert a new entry into a table; or propagate information about network conditions to other nodes. These background tasks don't need to be atomic. It is okay, and even desired, to preempt them with higher-priority tasks like packet processing. Further,background tasks might take a long time to finish, or they might not finish at all, e.g, periodically scanning through a counter table---forever.

In Lucid, we implement background threads like this with *recursive events*. Supporting multiple *data-plane threads* on line-rate hardware was a major initial goal of Lucid, which is why the file format for Lucid programs is `.dpt`.

In this example, `threads.dpt` there is a background thread that continuously scans for inactive flows and resets/exports their counters: 

```
    // background thread to export and timeout old entries.
    event checkIdx(t self, int idx);
    event clearIndex(t self, int idx);

    handle checkIdx(t self, int idx) {
        // Clear if it is not already cleared and timeout has elapsed.
        int lastTime = Array.get(self#lastUpdated, idx);
        if ((lastTime != 0) && (Sys.time() - lastTime < self#timeout)) {
            generate(clearIndex(self, idx));
        }
        int next_idx = (idx + 1 ) & (self#nFlows - 1);
        generate(Event.delay(checkIdx(self, next_idx), 1000000));
    }

    handle clearIndex(t self, int idx) {
        int oldSrc = ArrayUtils.replace(self#srcs, idx, 0);
        int oldDst = ArrayUtils.replace(self#dsts, idx, 0);    
        int oldCt = ArrayUtils.replace(self#pktCts, idx, 0);
        ArrayUtils.set(self#lastUpdated, idx, 0);
        event evictedRecord = flowRecord(oldSrc, oldDst, oldCt);
        generate_port((int<'p>)(self#collectorPort), evictedRecord);
    }
```

`checkIdx`'s handler checks if the flow in slot i has timed out, then generates a `checkIdx` for the next slot. Whenever a packet arrives, it will only have to wait for at most one `checkIdx` and `clearIdx` handler to complete, then it will be able to execute atomically. Of course, if there are other packets in the input queue, those will also be processed first. 

**Sleeping.** In multi-threaded applications, sleeping is important to give other threads access to unused cycles. High performance packet processing hardware often cannot "sleep". However, one common capability is to pause the generation or transmission of packets. Lucid abstracts this capability with `Event.delay`: a standard library function that delays the generation of an event by the specified time. In this example, we use `Event.delay` to make the checkIdx thread wait 1ms (1000000ns) between indices.
