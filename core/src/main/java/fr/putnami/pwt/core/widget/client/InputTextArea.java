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

import com.google.gwt.dom.client.TextAreaElement;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.TextArea;

import fr.putnami.pwt.core.widget.client.base.AbstractInputBox;
import fr.putnami.pwt.core.widget.client.helper.StringParser;
import fr.putnami.pwt.core.widget.client.helper.StringRenderer;

public class InputTextArea extends AbstractInputBox<TextArea, String> {

	private Integer rows;

	public InputTextArea() {
		super(new TextArea());

		setParser(StringParser.get());
		setRenderer(StringRenderer.get());
	}

	protected InputTextArea(InputTextArea source) {
		super(new TextArea(), source);
		setMaxLength(source.getMaxLength());
		if (source.rows != null) {
			setRows(source.rows);
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new InputTextArea(this);
	}

	@Override
	public void setMaxLength(int maxLength) {
		if (maxLength != -1) {
			getElement().setAttribute("maxLength", "" + maxLength);
		}
	}

	@Override
	public int getMaxLength() {
		String maxLengthAtt = TextAreaElement.as(getElement()).getAttribute("maxLength");
		if (maxLengthAtt != null && maxLengthAtt.length() > 0) {
			return Integer.valueOf(maxLengthAtt);
		}
		return -1;
	}

	public void setRows(int rows) {
		this.rows = rows;
		TextAreaElement.as(getElement()).setRows(rows);
	}

	public int setRows() {
		return TextAreaElement.as(getElement()).getRows();
	}

}
