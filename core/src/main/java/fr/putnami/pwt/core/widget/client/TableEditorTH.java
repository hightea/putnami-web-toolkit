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

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import java.util.Collection;

import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.EditorLabel;
import fr.putnami.pwt.core.editor.client.Path;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.base.AbstractTableColumnAspect;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class TableEditorTH<T> extends TableTH<T> implements EditorLabel {

	private static final CssStyle STYLE_FLOAT_RIGHT = new SimpleStyle("pull-right");
	private Collection<AbstractTableColumnAspect<T>> aspects = Lists.newArrayList();
	private String text;

	public TableEditorTH() {
		super();
	}

	protected TableEditorTH(TableEditorTH<T> source) {
		super(source);
		for (AbstractTableColumnAspect<T> aspect : source.aspects) {
			this.addAspect(aspect.cloneAspect());
		}
	}

	@Override
	public IsWidget cloneWidget() {
		return new TableEditorTH<T>(this);
	}

	public void addAspect(AbstractTableColumnAspect<T> aspect) {
		this.aspects.add(aspect);
	}

	@Override
	public String getLabelKey() {
		return null;
	}

	@Override
	public String[] getSuffix() {
		return new String[] {EditorLabel.HEADER_SUFFIX, EditorLabel.EMPTY_SUFFIX};
	}

	@Override
	public boolean isLabelMandatory() {
		return !Path.ROOT_PATH.equals(this.getPath());
	}

	@Override
	public String getText() {
		return this.text;
	}

	@Override
	public void setText(String text) {
		this.text = text;
	}

	@Override
	public void redraw() {
		super.redraw();
		this.clear();
		if (!this.aspects.isEmpty()) {
			FlowPanel flowPanel = new FlowPanel();
			int countAspectWidget = 0;
			for (AbstractTableColumnAspect aspect : this.aspects) {
				Widget aspectWidget = aspect.asWidget();
				if (aspectWidget != null) {
					flowPanel.add(aspectWidget);
					countAspectWidget++;
				}
			}
			if (countAspectWidget > 0) {
				StyleUtils.addStyle(flowPanel, TableEditorTH.STYLE_FLOAT_RIGHT);
				this.append(flowPanel);
			}
		}
		if (this.text != null) {
			Label label = new Label();
			label.setText(this.text);
			this.append(label);
		}
	}

	@Override
	public Iterable<Editor> getEditors() {
		return Iterables.unmodifiableIterable((Collection) this.aspects);
	}

	@Override
	public void add(IsWidget w) {
		throw new UnsupportedOperationException("TableEditorTH does not support add(IsWidget) method");
	}
}
