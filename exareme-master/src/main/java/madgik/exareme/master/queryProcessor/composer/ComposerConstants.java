package madgik.exareme.master.queryProcessor.composer;

import org.apache.log4j.Logger;

/**
 * @author alex
 */
public class ComposerConstants {
    private static final Logger log = Logger.getLogger(ComposerConstants.class);

    public static final String algorithmKey = "algorithm_key";
    public static final String inputLocalDBKey = "input_local_DB";
    public static final String dbQueryKey = "db_query";
    public static final String inputGlobalTblKey = "input_global_tbl";
    public static final String outputGlobalTblKey = "output_tbl";
    public static final String prevOutputGlobalTblKey = "prv_output_global_tbl";
    public static final String prevOutputLocalTblKey = "prv_output_local_tbl";
    public static final String defaultDBKey = "defaultDB";
    public static final String dbIdentifierKey = "dbIdentifier";
    public static final String DFL_SCRIPT_FILE_EXTENSION = ".dfl";
    public static final String localDBsKey = "local_step_dbs";
    public static final String globalDBKey = "global_step_db";
    public static final String curStatePKLKey = "cur_state_pkl";
    public static final String prevStatePKLKey = "prev_state_pkl";

}
