package fr.putnami.pwt.plugin.ajaxbot.util;

import java.io.File;

public final class AjaxBotUtils {

	public static final String FILTER_PARAM_CACHE_FOLDER = "cacheFolder";

	public static final String QUERY_PARAM_ESCAPED_FRAGMENT = "_escaped_fragment_";
	public static final String QUERY_PARAM_RESET_FILTER = "_ajaxbotfilter_cache_reset_";

	private AjaxBotUtils() {
	}

	public static void deleteFolder(File folder) {
		if (folder == null) {
			return;
		}
		File[] files = folder.listFiles();
		if (files != null) { // some JVMs return null for empty dirs
			for (File f : files) {
				if (f.isDirectory()) {
					deleteFolder(f);
				} else {
					f.delete();
				}
			}
		}
		folder.delete();
	}

}
