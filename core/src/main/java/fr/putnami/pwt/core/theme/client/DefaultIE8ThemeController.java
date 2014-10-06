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
package fr.putnami.pwt.core.theme.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.ScriptElement;

public class DefaultIE8ThemeController extends DefaultThemeController {

	private static final String RESPOND_JS_LOCATION = "theme/default/script/respond.min.js";

	private ScriptElement respondJsScript;

	@Override
	public void resetTheme() {
		this.ensureRespondJsScriptElement();
		this.respondJsScript.removeFromParent();
		super.resetTheme();
		this.getHead().appendChild(this.respondJsScript);
	}

	private void ensureRespondJsScriptElement() {
		if (this.respondJsScript == null) {
			this.respondJsScript = Document.get().createScriptElement();
			this.respondJsScript.setSrc(GWT.getModuleBaseForStaticFiles()
					+ DefaultIE8ThemeController.RESPOND_JS_LOCATION);
			this.respondJsScript.setType("text/javascript");
		}
	}

}
