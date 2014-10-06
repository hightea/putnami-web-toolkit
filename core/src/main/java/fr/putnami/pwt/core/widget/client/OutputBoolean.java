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

import com.google.gwt.dom.client.Document;
import com.google.gwt.dom.client.Text;
import com.google.gwt.uibinder.client.UiChild;
import com.google.gwt.user.client.Element;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractOutput;

public class OutputBoolean extends AbstractOutput<Boolean> {

	public enum RenderType {
		ICON_AND_TEXT, ICON, TEXT;
	}

	private String trueLabel = Boolean.TRUE.toString();
	private String falseLabel = Boolean.FALSE.toString();

	private Icon trueIcon;
	private Icon falseIcon;

	private RenderType outputType = RenderType.ICON_AND_TEXT;

	public OutputBoolean() {
	}

	protected OutputBoolean(OutputBoolean source) {
		super(source);
		this.trueLabel = source.trueLabel;
		this.falseLabel = source.falseLabel;
		this.outputType = source.outputType;
		if (source.trueIcon != null) {
			this.setTrueIcon((Icon) source.trueIcon.cloneWidget());
		}
		if (source.falseIcon != null) {
			this.setFalseIcon((Icon) source.falseIcon.cloneWidget());
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputBoolean(this);
	}

	@Override
	protected void ensureElement(Element element) {
		this.resetInner();
	}

	@Override
	protected void renderValue(Boolean value) {
		this.resetInner();
	}

	private void resetInner() {
		if (this.elementExists()) {
			Element element = this.getElement();
			element.removeAllChildren();
			boolean rendervalue = Boolean.TRUE.equals(this.getValue());
			if (this.outputType != RenderType.TEXT) {
				Icon icon = rendervalue ? this.trueIcon : this.falseIcon;
				if (icon != null) {
					element.appendChild(icon.getElement());
				}
			}
			if (this.outputType != RenderType.ICON) {
				Text textElem =
						Document.get().createTextNode(rendervalue ? this.trueLabel : this.falseLabel);
				element.appendChild(textElem);
			}
		}
	}

	public String getTrueLabel() {
		return this.trueLabel;
	}

	public void setTrueLabel(String trueLabel) {
		this.trueLabel = trueLabel;
		this.resetInner();
	}

	public String getFalseLabel() {
		return this.falseLabel;
	}

	public void setFalseLabel(String falseLabel) {
		this.falseLabel = falseLabel;
		this.resetInner();
	}

	@UiChild(tagname = "iconTrue", limit = 1)
	public void setTrueIcon(Icon icon) {
		this.trueIcon = icon;
		this.resetInner();
	}

	public void setTrueIconType(String iconType) {
		this.trueIcon = new Icon();
		this.trueIcon.setType(iconType);
		this.resetInner();
	}

	@UiChild(tagname = "iconFalse", limit = 1)
	public void setFalseIcon(Icon icon) {
		this.falseIcon = icon;
		this.resetInner();
	}

	public void setFalseIconType(String iconType) {
		this.falseIcon = new Icon();
		this.falseIcon.setType(iconType);
		this.resetInner();
	}

	public void setOutputType(RenderType outputType) {
		this.outputType = outputType;
		this.resetInner();
	}

}
