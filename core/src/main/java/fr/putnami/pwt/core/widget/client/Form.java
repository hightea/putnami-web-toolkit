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

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.validator.Validator;
import fr.putnami.pwt.core.widget.client.base.AbstractForm;
import fr.putnami.pwt.core.widget.client.base.CssStyle;
import fr.putnami.pwt.core.widget.client.base.HasFooter;
import fr.putnami.pwt.core.widget.client.base.HasHeader;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class Form<T> extends AbstractForm<T> implements
		HasHeader,
		HasFooter
{

	private static final CssStyle STYLE_FORM = new SimpleStyle("form");

	private Header header;
	private Footer footer;

	public Form(String html) {
		super(DivElement.TAG, html);
		StyleUtils.addStyle(this, STYLE_FORM);
		getElement().setAttribute("role", "form");
	}

	protected Form(Form<T> source) {
		super(source);
		getElement().setAttribute("role", "form");
	}

	@Override
	public IsWidget cloneWidget() {
		return new Form<T>(this);
	}

	@Override
	public void addAndReplaceElement(Widget widget, com.google.gwt.user.client.Element toReplace) {
		if (widget instanceof Header) {
			assert this.header == null : "Header may only be set once";
			this.header = (Header) widget;
		}
		if (widget instanceof Footer) {
			assert this.footer == null : "Footer may only be set once";
			this.footer = (Footer) widget;
		}
		super.addAndReplaceElement(widget, toReplace);
	}

	@Override
	public Header getHeader() {
		return header;
	}

	@Override
	public Footer getFooter() {
		return this.footer;
	}

	@Override
	public void addValidator(Validator<T> validator) {
	}
}
