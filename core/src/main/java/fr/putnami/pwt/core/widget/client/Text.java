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

import com.google.gwt.dom.client.SpanElement;
import com.google.gwt.user.client.ui.HasText;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractWidget;
import fr.putnami.pwt.core.widget.client.util.HTMLUtils;

public class Text extends AbstractWidget implements HasText {

	private final SpanElement span;
	private String text;

	public Text() {
		super(SpanElement.TAG);
		this.span = SpanElement.as(this.getElement());
	}

	public Text(String text) {
		this();
		this.setText(text);
	}

	public Text(Text source) {
		super(source);
		this.span = SpanElement.as(this.getElement());
		this.span.setInnerText(source.text);
	}

	@Override
	public IsWidget cloneWidget() {
		return new Text(this);
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = HTMLUtils.unescapeHTML(text);
		this.span.setInnerText(this.text == null ? "" : this.text);
	}

}
