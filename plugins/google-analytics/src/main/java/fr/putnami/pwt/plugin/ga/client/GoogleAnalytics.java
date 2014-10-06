/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.ga.client;

import com.google.common.collect.Maps;
import com.google.gwt.core.shared.GWT;

import java.util.Map;

public abstract class GoogleAnalytics {

	private static Map<String, GoogleAnalytics> cache = Maps.newHashMap();

	public static GoogleAnalytics get(String account) {
		GoogleAnalytics ga = cache.get(account);
		if (ga == null) {
			ga = GWT.create(GoogleAnalytics.class);
			ga.initialize(account);
		}
		return ga;
	}

	public static GoogleAnalytics init(String account, String domain) {
		GoogleAnalytics ga = cache.get(account);
		if (ga == null) {
			ga = GWT.create(GoogleAnalytics.class);
			ga.initialize(account, domain);
		}
		return ga;
	}

	protected abstract void initialize(String account);

	protected abstract void initialize(String account, String domain);

	public abstract void handleUncaughtException(boolean enable);

	public abstract void forceSSL(boolean force);

	public abstract void displayfeatures();

	public abstract void trackPage();

	public abstract void trackPage(String pageName);

	public abstract void trackEvent(String category, String action);

	public abstract void trackEvent(String category, String action, String label);

	public abstract void trackEvent(String category, String action, String label, int value);

	public abstract void trackException(String description, boolean fatal);

	public abstract void trackSocial(String socialNetwork, String socialAction, String socialtarget);
}
