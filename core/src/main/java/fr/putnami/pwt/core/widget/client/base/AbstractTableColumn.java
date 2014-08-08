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
package fr.putnami.pwt.core.widget.client.base;

import java.util.Collection;
import java.util.Collections;

import com.google.common.collect.Lists;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.Widget;

import fr.putnami.pwt.core.editor.client.factory.CloneableWidget;
import fr.putnami.pwt.core.model.client.base.HasReadonly;
import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.TableTH;

public abstract class AbstractTableColumn<T> implements
HasReadonly,
IsWidget,
HasResponsiveVisibility,
CloneableWidget {

	public enum Type implements CssStyle {
		DEFAULT(null),
		ACTION("table-action-column");

		private final String style;

		private Type(String style) {
			this.style = style;
		}

		@Override
		public String get() {
			return style;
		}
	}

	public static enum ColumnVisibility {
		VISIBLE,
		HIDE,
		HIDE_READONLY,
		VISIBLE_READONLY;
	}

	private Collection<AbstractTableColumnAspect<T>> aspects;

	private Boolean readonly;
	private Integer colspan;

	private Visibility xsVisibility = Visibility.DEFAULT;
	private Visibility smVisibility = Visibility.DEFAULT;
	private Visibility mdVisibility = Visibility.DEFAULT;
	private Visibility lgVisibility = Visibility.DEFAULT;
	private Visibility printVisibility = Visibility.DEFAULT;

	private Type type = Type.DEFAULT;

	private ColumnVisibility columnVisibility = ColumnVisibility.VISIBLE;

	public AbstractTableColumn() {

	}

	protected AbstractTableColumn(AbstractTableColumn<T> source) {
		this.readonly = source.readonly;
		this.colspan = source.colspan;
		this.columnVisibility = source.columnVisibility;
		this.type = source.type;
	}

	@Override
	public Boolean getReadonly() {
		return readonly;
	}

	@Override
	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}

	public void setColspan(Integer colspan) {
		this.colspan = colspan;
	}

	public Integer getColspan() {
		return colspan;
	}

	public ColumnVisibility getColumnVisibility() {
		return columnVisibility;
	}

	public void setColumnVisibility(ColumnVisibility ColumnVisibility) {
		this.columnVisibility = ColumnVisibility;
	}

	public Collection<AbstractTableColumnAspect<T>> getAspects() {
		return aspects == null ? Collections.EMPTY_LIST : Collections.unmodifiableCollection(aspects);
	}

	public <A extends AbstractTableColumnAspect<T>> A getAspect(Class<A> aspectClass) {
		if (aspects != null) {
			for (AbstractTableColumnAspect<T> aspect : aspects) {
				if (aspectClass != null && aspectClass.equals(aspect.getClass())) {
					return (A) aspect;
				}
			}
		}
		return null;
	}

	public void addAspect(AbstractTableColumnAspect<T> aspect) {
		if (aspects == null) {
			aspects = Lists.newArrayList();
		}
		aspects.add(aspect);
	}

	@Override
	public Widget asWidget() {
		throw new UnsupportedOperationException("An AbstractTableColumn cannot be use as a widget. It exists for use in UiBinder Only");
	}

	@Override
	public void setXsVisibility(Visibility xsVisibility) {
		this.xsVisibility = xsVisibility;
	}

	@Override
	public void setSmVisibility(Visibility smVisibility) {
		this.xsVisibility = smVisibility;
	}

	@Override
	public void setMdVisibility(Visibility mdVisibility) {
		this.xsVisibility = mdVisibility;
	}

	@Override
	public void setLgVisibility(Visibility lgVisibility) {
		this.xsVisibility = lgVisibility;
	}

	@Override
	public void setPrintVisibility(Visibility printVisibility) {
		this.xsVisibility = printVisibility;
	}

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public final AbstractTableCell<T> createBodyCell() {
		AbstractTableCell<T> cell = doCreateBodyCell();
		setResponsiveVisibility(cell);
		cell.setReadonlyVisibility(columnVisibility);
		cell.setType(type);
		return cell;
	}

	public final TableTH<T> createHeaderCell() {
		TableTH<T> cell = doCreateHeaderCell();
		setResponsiveVisibility(cell);
		cell.setType(type);
		cell.setReadonlyVisibility(columnVisibility);
		return cell;
	}

	private void setResponsiveVisibility(HasResponsiveVisibility target) {
		target.setXsVisibility(xsVisibility);
		target.setSmVisibility(smVisibility);
		target.setMdVisibility(mdVisibility);
		target.setLgVisibility(lgVisibility);
		target.setPrintVisibility(printVisibility);
	}

	protected abstract AbstractTableCell<T> doCreateBodyCell();

	protected abstract TableTH<T> doCreateHeaderCell();
}
