/**
 * This file is part of pwt-google-analytics.
 *
 * pwt-google-analytics is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-google-analytics is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-google-analytics.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.ga.client;

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Element;
import com.google.gwt.dom.client.ScriptElement;
import com.google.gwt.user.client.Window;

import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.mvp.client.event.StartActivityEvent;

public class GoogleAnalyticsImpl extends GoogleAnalytics implements StartActivityEvent.Handler {

	private static final String SCRIPT_HTTP = "http://www.google-analytics.com/ga.js";
	private static final String SCRIPT_HTTPS = "https://ssl.google-analytics.com/ga.js";

	private static boolean jsLoaded = false;

	private void insertFirstScriptElement(ScriptElement script) {
		Element firstScript = Document.get().getElementsByTagName("script").getItem(0);
		firstScript.getParentNode().insertBefore(script, firstScript);
	}

	private void loadGoogleAnalyticsScript() {
		if (jsLoaded) {
			return;
		}
		jsLoaded = true;
		ScriptElement script = Document.get().createScriptElement();
		boolean secured = "https:".equals(Window.Location.getProtocol());
		script.setSrc(secured ? SCRIPT_HTTPS : SCRIPT_HTTP);
		script.setType("text/javascript");
		script.setAttribute("async", "true");

		insertFirstScriptElement(script);
	}

	@Override
	protected void initialize(String account) {
		String gaSession = "var __ga = __ga || [];__ga.push(['_setAccount', '" + account + "']);";
		ScriptElement config = Document.get().createScriptElement(gaSession);
		loadGoogleAnalyticsScript();
		insertFirstScriptElement(config);
		loadGoogleAnalyticsScript();
		MvpController.get().addStartActivityHandler(this);
		trackPage();
	}

	@Override
	public void onStartActivity(StartActivityEvent event) {
		String placeToken = MvpController.get().getToken(event.getPlace());
		trackPlace("/" + placeToken);
	}

	@Override
	public native void trackPlace(String placeName)
	/*-{
	    $wnd.__ga.push([ '_trackPageview', placeName ]);
	}-*/;

	@Override
	public native void trackPage()
	/*-{
	    $wnd.__ga.push([ '_trackPageview' ]);
	}-*/;

}
