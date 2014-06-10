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

import java.io.IOException;

import com.google.gwt.text.shared.Renderer;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.widget.client.base.AbstractOutput;

public class OutputBoolean extends AbstractOutput<Boolean> implements Renderer<Boolean> {

	private String trueLabel = Boolean.TRUE.toString();
	private String falseLabel = Boolean.TRUE.toString();

	public OutputBoolean() {
		setRenderer(this);
	}

	protected OutputBoolean(OutputBoolean source) {
		super(source);
	}

	@Override
	public IsWidget cloneWidget() {
		return new OutputBoolean(this);
	}

	@Override
	public String render(Boolean object) {
		return Boolean.TRUE.equals(object) ? trueLabel : falseLabel;
	}

	@Override
	public void render(Boolean object, Appendable appendable) throws IOException {
		appendable.append(render(object));
	}

	public String getTrueLabel() {
		return trueLabel;
	}

	public void setTrueLabel(String trueLabel) {
		this.trueLabel = trueLabel;
	}

	public String getFalseLabel() {
		return falseLabel;
	}

	public void setFalseLabel(String falseLabel) {
		this.falseLabel = falseLabel;
	}

}
