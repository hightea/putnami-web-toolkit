/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.ajaxbot.util;

import java.io.File;

public final class AjaxBotUtils {

	public static final String FILTER_PARAM_CACHE_FOLDER = "cacheFolder";
	public static final String FILTER_PARAM_SERVER_URL = "serverUrl";

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
