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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.dom.client.Document;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.SimplePanel;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;
import fr.putnami.pwt.core.widget.client.util.WidgetUtils;

public class BaseSimplePanel extends SimplePanel implements CloneableWidget {

	private final String tagName;

	public BaseSimplePanel(String tagName) {
		super(Document.get().createElement(tagName));
		this.tagName = tagName;
		StyleUtils.initStyle(this);
	}

	public BaseSimplePanel(BaseSimplePanel source) {
		this(source.tagName);
		this.setWidget(WidgetUtils.cloneWidget(this.getWidget()));
		StyleUtils.cloneStyle(this, source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new BaseSimplePanel(this);
	}
}
