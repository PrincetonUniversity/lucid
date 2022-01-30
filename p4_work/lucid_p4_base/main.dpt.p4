#include <core.p4>
#include <tna.p4>
control my_ingress_function(inout bit<32> x,inout bit<32> y,inout bit<32> retval) {
 
    action dpt_151_alu_0_opstmt( ){
        retval = y + x;
        
    }
    
    action dpt_151_merged_acn_1_acn_0_opstmt( ){
        dpt_151_alu_0_opstmt();
        //next tables: []
        
    }
    
    table dpt_1_merged_tbl {
        actions = {
            dpt_151_merged_acn_1_acn_0_opstmt;
        }
        const default_action = dpt_151_merged_acn_1_acn_0_opstmt();
        
    }
    
	apply{
 
    dpt_1_merged_tbl.apply();
	}
}
control my_egress_function(inout bit<32> a,inout bit<32> b,inout bit<32> c) {
 
    action dpt_237_alu_0_opstmt( ){
        c = b + a;
        
    }
    
    action dpt_237_merged_acn_2_acn_0_opstmt( ){
        dpt_237_alu_0_opstmt();
        //next tables: []
        
    }
    
    table dpt_2_merged_tbl {
        actions = {
            dpt_237_merged_acn_2_acn_0_opstmt;
        }
        const default_action = dpt_237_merged_acn_2_acn_0_opstmt();
        
    }
    
	apply{
 
    dpt_2_merged_tbl.apply();
	}
}