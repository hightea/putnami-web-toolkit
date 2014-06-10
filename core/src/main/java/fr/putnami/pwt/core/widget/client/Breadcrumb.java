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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.OListElement;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.base.AbstractPanel;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Breadcrumb extends AbstractPanel implements CloneableWidget {

	private static final CssStyle STYLE_BREADCRUMB = new SimpleStyle("breadcrumb");

	public Breadcrumb() {
		super(OListElement.TAG);
		endConstruct();
	}

	protected Breadcrumb(Breadcrumb source) {
		super(source);
		cloneSourceWidgets(source);
		endConstruct();
	}

	private void endConstruct() {
		StyleUtils.addStyle(this, STYLE_BREADCRUMB);
	}

	@Override
	public IsWidget cloneWidget() {
		return new Breadcrumb(this);
	}

	@Override
	public void add(IsWidget w) {
		if (w instanceof Nav.IsNavContent) {
			append(w);
		}
	}
}
