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

import com.google.gwt.dom.client.DivElement;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.EditorOutput;
import fr.putnami.pwt.core.widget.client.base.AbstractHTMLPanel;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class OutputGroup<T> extends AbstractHTMLPanel implements EditorOutput<T> {

	private T value;

	public OutputGroup(String html) {
		super(DivElement.TAG, html);
		StyleUtils.addStyle(this, EditorOutput.STYLE_CONTROL_STATIC);
	}

	protected OutputGroup(OutputGroup source) {
		super(source);
		StyleUtils.addStyle(this, EditorOutput.STYLE_CONTROL_STATIC);
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputGroup<T>(this);
	}

	@Override
	public T getValue() {
		return this.value;
	}

	@Override
	public void edit(T value) {
		this.value = value;
	}

	@Override
	public void addAndReplaceElement(Widget widget, Element toReplace) {
		super.addAndReplaceElement(widget, toReplace);
		StyleUtils.removeStyle(widget, EditorOutput.STYLE_CONTROL_STATIC);
	}
}
