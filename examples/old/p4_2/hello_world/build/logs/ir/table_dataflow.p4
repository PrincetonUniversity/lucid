action dpt_88_acn_90_generate_0_extra_processing( ){
    dpt_88_alu_89_generate_0_extra_processing();
    //next tables: []
    
}

action dpt_92_acn_94_generate_0_continue( ){
    dpt_92_alu_93_generate_0_continue();
    //next tables: []
    
}
// Stage not set by dptc
table dpt_88_generate_0_extra_processing {
    key = {
        md.dptMeta.eventType : ternary;
    }
    actions = {
        dpt_88_acn_90_generate_0_extra_processing;
    }
    //no default action
    const entries = {
        (1) : dpt_88_acn_90_generate_0_extra_processing();
        (2) : NOOP();
    }
    
}

// Stage not set by dptc
table dpt_92_generate_0_continue {
    key = {
        md.dptMeta.eventType : ternary;
    }
    actions = {
        dpt_92_acn_94_generate_0_continue;
    }
    //no default action
    const entries = {
        (1) : NOOP();
        (2) : dpt_92_acn_94_generate_0_continue();
    }
    
}
