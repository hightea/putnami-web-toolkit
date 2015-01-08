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
package fr.putnami.pwt.core.widget.client;

import com.google.gwt.dom.client.AnchorElement;
import com.google.gwt.dom.client.Document;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.theme.client.IconFont;
import fr.putnami.pwt.core.widget.client.base.AbstractTextOutput;
import fr.putnami.pwt.core.widget.client.helper.StringRenderer;

public class OutputEmail extends AbstractTextOutput<String> {

	public OutputEmail() {
		this.setRenderer(StringRenderer.get());
	}

	protected OutputEmail(OutputEmail source) {
		super(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputEmail(this);
	}

	@Override
	public void edit(String value) {
		this.getElement().removeAllChildren();
		if (value != null) {
			AnchorElement anchor = Document.get().createAnchorElement();
			Icon icon = new Icon();
			icon.setType(IconFont.ICON_MAIL);
			StringBuffer sb = new StringBuffer();
			sb.append("mailto:").append(value);
			anchor.setHref(sb.toString());
			anchor.appendChild(icon.getElement());
			anchor.appendChild(Document.get().createTextNode(value));

			this.getElement().appendChild(anchor);
		}
	}
}
