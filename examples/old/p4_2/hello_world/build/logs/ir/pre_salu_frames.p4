control DptEngine(inout dpt_meta_h md.dptMeta, inout dpt_args_h md.dptArgs){
    action dpt_88_alu_89_generate_0_extra_processing( ){
        hdr.dpt_extra_processing_out.setValid();
        md.dptMeta.nextEventType = 2;
        hdr.dpt_extra_processing_out.dpt_82_eventType = 2;
        hdr.dpt_extra_processing_out.dpt_74_ip = 0;
        
    }
    
    action dpt_92_alu_93_generate_0_continue( ){
        md.dptMeta.exitEventType = 3;
        md.dpt_continue.eventType = 3;
        md.dpt_continue.ip = hdr.dpt_extra_processing_in.dpt_39_ip;
        
    }
    action dpt_76_pktin( ){
        //next tables: [generate~0.extra_processing~88]
        
    }
    
    action dpt_77_extra_processing( ){
        //next tables: [generate~0.continue~92]
        
    }
    
    action dpt_88_acn_90_generate_0_extra_processing( ){
        dpt_88_alu_89_generate_0_extra_processing();
        //next tables: []
        
    }
    
    action dpt_92_acn_94_generate_0_continue( ){
        dpt_92_alu_93_generate_0_continue();
        //next tables: []
        
    }
    // Stage not set by dptc
    table dpt_0_selectEventType {
        key = {
            md.dptMeta.eventType : ternary;
        }
        actions = {
            dpt_76_pktin;
            dpt_77_extra_processing;
        }
        //no default action
        const entries = {
            (1) : dpt_76_pktin();
            (2) : dpt_77_extra_processing();
        }
        
    }
    
    // Stage not set by dptc
    table dpt_88_generate_0_extra_processing {
        actions = {
            dpt_88_acn_90_generate_0_extra_processing;
        }
        const default_action = dpt_88_acn_90_generate_0_extra_processing();
        
    }
    
    // Stage not set by dptc
    table dpt_92_generate_0_continue {
        actions = {
            dpt_92_acn_94_generate_0_continue;
        }
        const default_action = dpt_92_acn_94_generate_0_continue();
        
    }
    
    apply {
        switch (dpt_0_selectEventType.apply().action_run) {
            dpt_76_pktin : {
                dpt_88_generate_0_extra_processing.apply();
            }
            dpt_77_extra_processing : {
                dpt_92_generate_0_continue.apply();
            }
            
        }
    }
}