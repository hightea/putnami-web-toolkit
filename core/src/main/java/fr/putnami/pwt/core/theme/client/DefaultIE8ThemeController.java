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
package fr.putnami.pwt.core.theme.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.ScriptElement;

public class DefaultIE8ThemeController extends DefaultThemeController {

	private static String RESPOND_JS_LOCATION = "theme/default/script/respond.min.js";

	private ScriptElement respondJsScript;

	@Override
	public void resetTheme() {
		ensureRespondJsScriptElement();
		respondJsScript.removeFromParent();
		super.resetTheme();
		getHead().appendChild(respondJsScript);
	}

	private void ensureRespondJsScriptElement() {
		if (respondJsScript == null) {
			respondJsScript = Document.get().createScriptElement();
			respondJsScript.setSrc(GWT.getModuleBaseForStaticFiles() + RESPOND_JS_LOCATION);
			respondJsScript.setType("text/javascript");
		}
	}

}
